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

import cligen, terminal, os, strutils
import omni_dsp56300

const 
    NimblePkgVersion {.strdefine.} = ""
    omni_ver = NimblePkgVersion

let version_flag = "OmniDSP - version " & $omni_ver & "\nMotorola DSP56300 Assembly Compiler for Omni"

template printError(msg : string) : untyped =
    setForegroundColor(fgRed)
    writeStyled("ERROR: ", {styleBright}) 
    setForegroundColor(fgWhite, true)
    writeStyled(msg & "\n")

template printDone(msg : string) : untyped =
    if not silent:
        setForegroundColor(fgGreen)
        writeStyled("SUCCESS: ", {styleBright}) 
        setForegroundColor(fgWhite, true)
        writeStyled(msg & "\n")

proc omnidsp_single_file(is_multi : bool = false, fileFullPath : string, outName : string = "", outDir : string = "", silent : bool = false) : int =
    var 
        omniFile     = splitFile(fileFullPath)
        omniFileName = omniFile.name
        omniFileExt  = omniFile.ext
        omniFileDir  = omniFile.dir

    if not(omniFileExt == ".omni") and not(omniFileExt == ".oi"):
        printError($fileFullPath & " is not an Omni file.")
        return 1

    var outDirFullPath : string
    if outDir == "":
        outDirFullPath = omniFileDir
    else:
        outDirFullPath = outDir.normalizedPath().expandTilde().absolutePath()

    if not outDirFullPath.dirExists():
        printError("outDir: " & $outDirFullPath & " does not exist.")
        return 1

    try:
        let m = parseOmniFile(fileFullPath)
        let asmCode = emitDsp56300Asm(m)
        var asmOutName = if outName != "": outName else: omniFileName & ".asm"
        if not asmOutName.endsWith(".asm"):
            asmOutName.add(".asm")
        let targetFilePath = outDirFullPath / asmOutName
        writeFile(targetFilePath, asmCode)
        printDone("'" & asmOutName & "' has been generated to folder \"" & $outDirFullPath & "\".")
        return 0
    except Exception as e:
        printError("Failed to compile " & fileFullPath & ": " & e.msg)
        return 1

proc omnidsp(files : seq[string], outName : string = "", output : string = "", outDir : string = "", silent : bool = false) : int =
    if files.len == 0:
        printError("No Omni files or folders specified.")
        return 1

    let effOutName = if output != "": output else: outName

    if files.len == 1:
        let 
            omniFile         = files[0]
            omniFileFullPath = omniFile.normalizedPath().expandTilde().absolutePath()

        if omniFileFullPath.fileExists():
            return omnidsp_single_file(false, omniFileFullPath, effOutName, outDir, silent)
        elif omniFileFullPath.dirExists():
            for file in walkDir(omniFileFullPath):
                if file.kind == pcFile:
                    let dirFileFullPath = file.path.normalizedPath().expandTilde().absolutePath()
                    if dirFileFullPath.endsWith(".omni") or dirFileFullPath.endsWith(".oi"):
                        if omnidsp_single_file(true, dirFileFullPath, "", outDir, silent) > 0:
                            return 1
            return 0
        else:
            printError("Path: " & $omniFileFullPath & " does not exist.")
            return 1
    else:
        for omniFile in files:
            let omniFileFullPath = omniFile.normalizedPath().expandTilde().absolutePath()
            if omniFileFullPath.fileExists():
                if omnidsp_single_file(true, omniFileFullPath, "", outDir, silent) > 0:
                    return 1
            elif omniFileFullPath.dirExists():
                for file in walkDir(omniFileFullPath):
                    if file.kind == pcFile:
                        let dirFileFullPath = file.path.normalizedPath().expandTilde().absolutePath()
                        if dirFileFullPath.endsWith(".omni") or dirFileFullPath.endsWith(".oi"):
                            if omnidsp_single_file(true, dirFileFullPath, "", outDir, silent) > 0:
                                return 1
            else:
                printError("Path: " & $omniFileFullPath & " does not exist.")
                return 1
        return 0

clCfg.version = version_flag
clCfg.helpSyntax = ""
let arguments = "Arguments:\n  Omni file(s) or folder."
clCfg.hTabCols = @[ clOptKeys, clDflVal, clDescrip ]

dispatch(
    omnidsp,
    noHdr = true,
    usage = version_flag & "\n\n" & arguments & "\n\nOptions:\n$options",
    short = {
        "version" : 'v',
        "outName" : 'n',
        "output" : 'o',
        "outDir" : 'd',
        "silent" : 's'
    },
    help = {
        "help" : "CLIGEN-NOHELP",
        "version" : "CLIGEN-NOHELP",
        "outName" : "Name for the output assembly file. Defaults to the name of the input file.",
        "output" : "Output file path or name (alias for -n / --outName).",
        "outDir" : "Output folder. Defaults to the one of the Omni file(s) to compile.",
        "silent" : "CLIGEN-NOHELP"
    }
)
