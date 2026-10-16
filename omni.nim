# MIT License
# 
# Copyright (c) 2020-2021 Francesco Cameli
# 
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
# 
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

import cligen, terminal, os, strutils, osproc
import omni_dsp56300

#Package version is passed as argument when building. It will be constant and set correctly
const 
    NimblePkgVersion {.strdefine.} = ""
    omni_ver = NimblePkgVersion

#-v / --version
let version_flag = "Omni - version " & $omni_ver & "\n(c) 2020-2021 Francesco Cameli"

#Path to omni_lang
proc getOmniLangPkgPath(): string =
    let nimble_dir = if existsEnv("NIMBLE_DIR"): getEnv("NIMBLE_DIR")
                     else: "~/.nimble"
    let p1 = (nimble_dir & "/pkgs/omni_lang-" & omni_ver & "/omni_lang").normalizedPath().expandTilde()
    if dirExists(p1): return p1
    let p2 = (nimble_dir & "/pkgs2/omni_lang-" & omni_ver & "/omni_lang").normalizedPath().expandTilde()
    if dirExists(p2): return p2
    let pkgs2Dir = (nimble_dir & "/pkgs2").normalizedPath().expandTilde()
    if dirExists(pkgs2Dir):
        for kind, path in walkDir(pkgs2Dir):
            if kind == pcDir:
                let base = path.extractFilename()
                if base.startsWith("omni_lang-") or base.startsWith("omni_lang"):
                    let candidate = path / "omni_lang"
                    if dirExists(candidate):
                        return candidate
    let pkgsDir = (nimble_dir & "/pkgs").normalizedPath().expandTilde()
    if dirExists(pkgsDir):
        for kind, path in walkDir(pkgsDir):
            if kind == pcDir:
                let base = path.extractFilename()
                if base.startsWith("omni_lang-") or base.startsWith("omni_lang"):
                    let candidate = path / "omni_lang"
                    if dirExists(candidate):
                        return candidate
    return p1

let omni_lang_pkg_path = getOmniLangPkgPath()

#Extension for static lib
when defined(Linux):
    const 
        lib_prepend          = "lib"
        static_lib_extension = ".a"
        shared_lib_extension = ".so"
        default_compiler     = "gcc"

when defined(MacOSX) or defined(MacOS):
    const 
        lib_prepend          = "lib"
        static_lib_extension = ".a"
        shared_lib_extension = ".dylib"
        default_compiler     = "clang"

when defined(Windows):
    const 
        lib_prepend          = ""           #Windows doesn't prepend "lib" to libraries
        static_lib_extension = ".lib"
        shared_lib_extension = ".dll"
        default_compiler     = "gcc(MinGW)"

#Generic error
template printError(msg : string) : untyped =
    setForegroundColor(fgRed)
    writeStyled("ERROR: ", {styleBright}) 
    setForegroundColor(fgWhite, true)
    writeStyled(msg & "\n")

#Generic success
template printDone(msg : string) : untyped =
    if not silent:
        setForegroundColor(fgGreen)
        writeStyled("SUCCESS: ", {styleBright}) 
        setForegroundColor(fgWhite, true)
        writeStyled(msg & "\n")

#Parse compilation output for Gc allocations and pretty print it with colors
proc parseAndPrintCompilationString(msg : string) : bool =
    #Turn Error: and Warning: into red bright(1m) Error: (ansi escape codes: https://forum.nim-lang.org/t/7002)
    var colored_msg = msg.multiReplace([("Error:", "\e[31;1mError:\e[0m"), ("Warning:", "\e[31;1mError:\e[0m")])
    echo colored_msg

    #Check GcMem. --warningAsError doesn't work correctly, as it would print error even when there is not!
    if msg.contains("GcMem"):
        printError("Trying to allocate memory through Nim's Garbage Collector. This is not allowed in Omni. Use 'Data' for all your allocations.")
        return true

    return false

#Actual compiler
proc omni_single_file(is_multi : bool = false, fileFullPath : string, outName : string = "", outDir : string = "", target : string = "", emitAsm : bool = false, compileOnly : bool = false, lib : string = "shared", architecture : string = "native",  compiler : string = default_compiler, performBits : string = "32/64", wrapper : string = "", define : seq[string] = @[], importModule : seq[string] = @[], passNim : seq[string] = @[],exportHeader : bool = true, exportIO : bool = false, silent : bool = false) : int =

    var 
        omniFile     = splitFile(fileFullPath)
        omniFileDir  = omniFile.dir
        omniFileName = omniFile.name
        omniFileExt  = omniFile.ext
    
    #Check file first charcter, must be a capital letter
    if not omniFileName[0].isUpperAscii:
        omniFileName[0] = omniFileName[0].toUpperAscii()

    #Check file extension
    if not(omniFileExt == ".omni") and not(omniFileExt == ".oi"):
        printError($fileFullPath & " is not an Omni file.")
        return 1
    
    var outDirFullPath : string

    #Check if outDir is empty. Use .omni's file path in that case.
    if outDir == "":
        outDirFullPath = omniFileDir
    else:
        outDirFullPath = outDir.normalizedPath().expandTilde().absolutePath()
    
    #Check if dir exists
    if not outDirFullPath.dirExists():
        printError("outDir: " & $outDirFullPath & " does not exist.")
        return 1

    # Check if target is dsp56300 or c, or emitAsm/compileOnly
    let effectiveTarget = if emitAsm: "dsp56300" elif compileOnly: "c" else: target.toLowerAscii()

    if effectiveTarget == "dsp56300" or effectiveTarget == "dsp56300asm":
        let m = parseOmniFile(fileFullPath)
        let asmCode = emitDsp56300Asm(m)
        var asmOutName = if outName != "": outName else: omniFileName & ".asm"
        if not asmOutName.endsWith(".asm"):
            asmOutName.add(".asm")
        let targetFilePath = outDirFullPath / asmOutName
        writeFile(targetFilePath, asmCode)
        printDone("'" & asmOutName & "' has been generated to folder \"" & $outDirFullPath & "\".")
        return 0

    elif effectiveTarget == "c":
        let m = parseOmniFile(fileFullPath)
        let cCode = emitC(m)
        var cOutName = if outName != "": outName else: omniFileName & ".c"
        if not cOutName.endsWith(".c"):
            cOutName.add(".c")
        let targetFilePath = outDirFullPath / cOutName
        writeFile(targetFilePath, cCode)
        printDone("'" & cOutName & "' has been generated to folder \"" & $outDirFullPath & "\".")
        return 0

    elif effectiveTarget == "asm":
        printError("Target 'asm' is retired and ambiguous. Use '--target:dsp56300', '--emitAsm', or the dedicated 'omnidsp' compiler.")
        return 1

    elif effectiveTarget != "":
        printError("Target '" & target & "' is invalid. Valid targets are 'dsp56300' (or 'dsp56300Asm') and 'c'.")
        return 1

    #Check performBits argument
    if performBits != "32" and performBits != "64" and performBits != "32/64":
        printError("performBits: " & $performBits & " is invalid. Valid values are '32', '64' and '32/64'.")
        return 1

    #Check lib argument
    var 
        lib_nim : string
        lib_extension : string
    
    if lib == "shared":
        lib_nim = "lib"
        lib_extension = shared_lib_extension
    elif lib == "static":
        lib_nim = "staticLib"
        lib_extension = static_lib_extension
    else:
        printError("Invalid --lib argument: \"" & $lib & "\". Use 'shared' to build a shared library, or 'static' to build a static one.")
        return 1

    #Set output name:
    var output_name : string
    if outName == "":
        output_name = $lib_prepend & $omniFileName & $lib_extension
    else:
        output_name = $lib_prepend & $outName & $lib_extension
    
    #CD into out dir. This is needed by nim compiler to do --app:staticLib due to this bug: https://github.com/nim-lang/Nim/issues/12745
    setCurrentDir(outDirFullPath)

    #If architecture == native, also pass the mtune=native flag.
    #If architecture == none, no architecture applied
    var real_architecture = ""
    if compiler == "clang" and architecture == "native" and hostCPU != "amd64":
        # clang on various non-x86 platforms doesn't like -march=native
        # see https://stackoverflow.com/questions/65966969/why-does-march-native-not-work-on-apple-m1
        real_architecture = ""
    else:
        real_architecture = "--passC:-march=" & $architecture
        if architecture == "native":
            real_architecture = real_architecture & " --passC:-mtune=native"
        #x86_64 / amd64 as aliases for x86-64
        elif architecture == "x86_64" or architecture == "amd64":
            real_architecture = "--passC:-march=x86-64"
        elif architecture == "none":
            real_architecture = ""

    #Add -d:lto only on Linux and Windows (not working on OSX + Clang yet: https://github.com/nim-lang/Nim/issues/15578)
    var lto = ""
    when defined(Linux):
        lto = "-d:lto"
    elif defined(Windows):
        #-ffat-lto-objects fixes issues with MinGW
        lto = "-d:lto --passC:\"-ffat-lto-objects\" --passL:\"-ffat-lto-objects\""
    
    #Actual compile command.
    var compile_command = 
        "nim c --out:" & output_name & " --app:" & lib_nim & 
        " --mm:none --noMain:on --panics:on --hints:off --checks:off --assertions:off" & 
        " --opt:speed -d:release -d:danger " & lto & " --passC:-fPIC " & real_architecture &
        " --warning[User]:off --warning[UnusedImport]:off --colors:off --stdout:on"

    #Fix for -d:lto not working yet on OSX + Clang: https://github.com/nim-lang/Nim/issues/15578
    when defined(MacOSX) or defined(MacOS):
        compile_command.add(" --passC:\"-flto\" --passL:\"-flto\"")

    #Add compiler info if not default compiler (which is passed in already from nim.cfg)
    if compiler != default_compiler:
        compile_command.add(" --cc:" & compiler)

    #Append additional definitions
    for new_define in define:
        #Look if -d has paths in it. Paths are expressed like so: -d:tempDir:"./"
        let split_define = new_define.split(':')
        
        #Standard case -d:danger
        if split_define.len() <= 1:
            compile_command.add(" -d:" & $new_define)
        
        #Normal and unix paths
        elif split_define.len() == 2:
            let 
                define_type  = split_define[0]
                define_path  = split_define[1]

            if define_path.contains('/') or define_path.contains('\\'):
                compile_command.add(" -d:" & $define_type & ":\"" & $define_path & "\"")

        #Windows has C:\\
        elif split_define.len() == 3:
            let define_type  = split_define[0]
            var define_path  = split_define[1]
            
            #Add the full path back
            define_path.add(":" & $(split_define[2]))

            if define_path.contains('/') or define_path.contains('\\'):
                compile_command.add(" -d:" & $define_type & ":\"" & $define_path & "\"")

    #Set performBits flag
    if performBits == "32":
        compile_command.add(" -d:omni_perform32")
    elif performBits == "64":
        compile_command.add(" -d:omni_perform64")
    else:
        compile_command.add(" -d:omni_perform32 -d:omni_perform64")

    #Always include local omni_lang path if it exists
    let localOmniLangPath = currentSourcePath().parentDir / "omni_lang"
    if localOmniLangPath.dirExists():
        compile_command.add(" --path:\"" & localOmniLangPath & "\"")

    #Import omni_lang first
    compile_command.add(" --import:omni_lang")
    
    #Check if a wrapper has been specified. If it is, import it
    if wrapper.isEmptyOrWhitespace.not:
        compile_command.add(" --import:\"" & wrapper & "\"")
    
    #Append additional imports
    for new_importModule in importModule:
        compile_command.add(" --import:\"" & $new_importModule & "\"")

    #Append additional flags for Nim compiler
    for new_nim_flag in passNim:
        compile_command.add(" " & $new_nim_flag)

    #Export IO
    var omni_io : string
    if exportIO:
        compile_command.add(" -d:omni_export_io -d:tempDir:\"" & $outDirFullPath & "\"")
        omni_io = outDirFullPath & "/omni_io.txt"

    #Finally, append the path to the actual omni file to compile:
    compile_command.add(" \"" & $fileFullPath & "\"")

    #Actually execute compilation. execCmdEx works fine on all OSes
    let (compilationString, failedOmniCompilation) = execCmdEx(compile_command)

    #Path to compiled shared / static lib
    let pathToCompiledLib = outDirFullPath & "/" & $output_name
    template removeCompiledLib() : untyped =
        removeFile(pathToCompiledLib)
        if exportIO: removeFile(omni_io)

    #Check for GcMem warnings and print errors out 
    if parseAndPrintCompilationString(compilationString):
        removeCompiledLib()
        if is_multi:
            printError("Failed compilation of '" & omniFileName & omniFileExt & "'.")
        return 1
    
    #Error code from execCmd is usually some 8bit number saying what error arises. It's not important for now.
    if failedOmniCompilation > 0:
        #No need to removeCompiledLib() as compilation failed anyway
        if is_multi:
            printError("Failed compilation of '" & omniFileName & omniFileExt & "'.")
        return 1

    #Check if Omni_UGenPerform32/64 are present, meaning perform/sample has been correctly specified. nm works with both shared and static libs!
    when not defined(Windows):
        let failedOmniCheckPerform = execCmd("nm \"" & $pathToCompiledLib & "\" | grep -q -F Omni_UGenPerform")            # -q == silent output
    else:
        let failedOmniCheckPerform = execShellCmd("nm \"" & $pathToCompiledLib & "\" | findstr Omni_UGenPerform >$null")   # >$null == silent output
        if fileExists("$null"):
            removeFile("$null")

    #grep / findstr returns 0 if it finds the string, 1 if it doesn't
    if failedOmniCheckPerform > 0:
        printError("Undefined 'perform' or 'sample' blocks.\n")
        removeCompiledLib()
        if is_multi:
            printError("Failed compilation of '" & omniFileName & omniFileExt & "'.")
        return 1

    #Export omni.h too
    if exportHeader:
        var omni_header_path = (omni_lang_pkg_path & "/core/omni.h").normalizedPath().expandTilde().absolutePath()
        if not omni_header_path.fileExists():
            let localHeader = currentSourcePath().parentDir / "omni_lang/omni_lang/core/omni.h"
            if localHeader.fileExists():
                omni_header_path = localHeader
        let omni_header_out_path = outDirFullPath & "/omni.h"
        if omni_header_path.fileExists():
            copyFile(omni_header_path, omni_header_out_path)

    #Done!
    printDone("'" & output_name & "' has been compiled to folder \"" & $outDirFullPath & "\".")

    return 0

#Unpack files arg and pass it to compiler
proc omni(files : seq[string], outName : string = "", output : string = "", outDir : string = "", target : string = "", emitAsm : bool = false, compileOnly : bool = false, lib : string = "shared", architecture : string = "native", compiler : string = default_compiler, performBits : string = "32/64", wrapper : string = "", define : seq[string] = @[], importModule : seq[string] = @[], passNim : seq[string] = @[], exportHeader : bool = true, exportIO : bool = false, silent : bool = false) : int =
    #no files provided, print --version
    if files.len == 0:
        echo version_flag
        return 0

    var effOutName = if output != "": output else: outName
    var effOutDir = outDir
    if effOutName != "":
        let (d, n, ext) = splitFile(effOutName)
        if d != "":
            effOutDir = d
            effOutName = n & ext

    for omniFile in files:
        #Get full extended path
        let omniFileFullPath = omniFile.normalizedPath().expandTilde().absolutePath()

        #If it's a file or list of files, compile it / them
        if omniFileFullPath.fileExists():
            #if just one file in CLI, also pass the outName flag
            if files.len == 1:
                return omni_single_file(false, omniFileFullPath, effOutName, effOutDir, target, emitAsm, compileOnly, lib, architecture, compiler, performBits, wrapper, define, importModule, passNim, exportHeader, exportIO, silent)
            else:
                if omni_single_file(true, omniFileFullPath, "", effOutDir, target, emitAsm, compileOnly, lib, architecture, compiler, performBits, wrapper, define, importModule, passNim, exportHeader, exportIO, silent) > 0:
                    return 1

        #If it's a dir, compile all .omni/.oi files in it
        elif omniFileFullPath.dirExists():
            for kind, dirFile in walkDir(omniFileFullPath):
                if kind == pcFile:
                    let 
                        dirFileFullPath = dirFile.normalizedPath().expandTilde().absolutePath()
                        dirFileExt = dirFileFullPath.splitFile().ext
                    
                    if dirFileExt == ".omni" or dirFileExt == ".oi":
                        if omni_single_file(true, dirFileFullPath, "", effOutDir, target, emitAsm, compileOnly, lib, architecture, compiler, performBits, wrapper, define, importModule, passNim, exportHeader, exportIO, silent) > 0:
                            return 1

        else:
            printError($omniFileFullPath & " does not exist.")
            return 1
    
    return 0

#Pass custom version string
clCfg.version = version_flag

#Remove --help-syntax
clCfg.helpSyntax = ""

#Arguments string
let arguments = "Arguments:\n  Omni file(s) or folder."

#Ignore clValType
clCfg.hTabCols = @[ clOptKeys, #[clValType,]# clDflVal, clDescrip ]

#Dispatch the omni function as the CLI one
dispatch(
    omni,

    #Remove "Usage: ..."
    noHdr = true,
    
    #Custom options printing
    usage = version_flag & "\n\n" & arguments & "\n\nOptions:\n$options",

    short = {
        "version" : 'v',
        "outName" : 'n',
        "output" : 'o',
        "target" : 't',
        "performBits" : 'b',
        "importModule" : 'm',
        "passNim" : 'p',
        "exportIO" : 'i'
    },
    
    help = {
        "help" : "CLIGEN-NOHELP",
        "version" : "CLIGEN-NOHELP",
        "outName" : "Name for the output library or file. Defaults to the name of the input file.",
        "output" : "Output file path or name (alias for -n / --outName).",
        "outDir" : "Output folder. Defaults to the one of the Omni file(s) to compile.",
        "target" : "Target compilation backend: 'dsp56300' / 'dsp56300Asm' (Motorola DSP56300 assembly) or 'c' (clean C99/C11 code).",
        "emitAsm" : "Emit Motorola DSP56300 assembly (.asm). Shorthand for --target:dsp56300.",
        "compileOnly" : "Emit self-contained C code (.c). Shorthand for --target:c.",
        "lib" : "Build a 'shared' or 'static' library.",
        "architecture" : "Build architecture.",
        "compiler" : "Select a different C backend compiler to use. Omni supports all of Nim's C compilers.",
        "performBits" : "Set precision for 'ins' and 'outs' in the perform block. Accepted values are '32', '64' or '32/64'. Note that this option does not affect Omni's internal floating point precision.",
        "wrapper" : "Specify an Omni wrapper to use.",
        "define" : "Define additional symbols for the intermediate Nim compiler.",
        "importModule" : "Import additional Nim modules to be compiled with the Omni file(s).",
        "passNim" : "Pass additional flags to the intermediate Nim compiler.",
        "exportHeader" : "Export the 'omni.h' header file together with the compiled lib.",
        "exportIO" : "Export the 'omni_io.txt' file together with the compiled lib.",
        "silent" : "CLIGEN-NOHELP"
    }
)
