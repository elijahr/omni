import std/[unittest, strutils]
import ../omni_dsp56300

suite "Omni DSP56300 & C Backend Pipeline":

  test "OmniSine translates to Motorola DSP56300 ASM and C":
    let m = parseOmniFile("examples/OmniSine.omni")
    check m.name == "OmniSine"
    check m.params.len == 1
    check m.params[0].name == "freq"
    check m.stateVars.len == 1
    check m.stateVars[0].name == "phase"

    let asmCode = emitDsp56300Asm(m)
    check "ORG     P:$0100" in asmCode
    check "ORG     X:$0020" in asmCode
    check "ORG     Y:$0000" in asmCode
    check "OmniSine_process:" in asmCode
    check "RTS" in asmCode
    check "freq" in asmCode
    check "phase" in asmCode
    check "out1" in asmCode

    let cCode = emitC(m)
    check "typedef struct {" in cCode
    check "OmniSine_process" in cCode
    check "OmniSine_alloc" in cCode
    check "OmniSine_free" in cCode
    check "sinf" in cCode
    check "fmodf" in cCode

  test "OmniSaw translates to Motorola DSP56300 ASM and C":
    let m = parseOmniFile("examples/OmniSaw.omni")
    check m.name == "OmniSaw"
    check m.params.len >= 1
    check m.stateVars.len >= 2

    let asmCode = emitDsp56300Asm(m)
    check "OmniSaw_process:" in asmCode
    check "RTS" in asmCode

    let cCode = emitC(m)
    check "OmniSaw_process" in cCode

  test "OmniSVF translates to Motorola DSP56300 ASM and C":
    let m = parseOmniFile("examples/OmniSVF.omni")
    check m.name == "OmniSVF"
    check m.params.len == 3
    check m.stateVars.len >= 2

    let asmCode = emitDsp56300Asm(m)
    check "OmniSVF_process:" in asmCode
    check "MAC" in asmCode
    check "RTS" in asmCode

    let cCode = emitC(m)
    check "OmniSVF_process" in cCode
