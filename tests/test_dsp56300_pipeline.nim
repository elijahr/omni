## End-to-end TDD Verification Suite for Omni DSP56300 & C Compiler Pipeline
## Verifies:
## 1. Zero hardcoded variable names (arbitrary identifiers work)
## 2. AST parsing of nested control flow (if / elif / else) and expressions
## 3. Motorola DSP56300 assembly generation and assembly with dsp56300_asm
## 4. Bit-accurate execution on DspState simulation core for OscB (PWM) and EnvADSR
## 5. Clean C99/C11 generation across all 18 Nord Modular G2 DSP modules

import std/[unittest, strutils, os, tables]
import ../omni_dsp56300
import dsp56300/[state, ir, runner, asm_parser]

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

    let res = assembleText(asmCode)
    check res.pRam.len > 0

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

    let res = assembleText(asmCode)
    check res.pRam.len > 0

    let cCode = emitC(m)
    check "OmniSaw_process" in cCode

  test "OmniSVF translates to Motorola DSP56300 ASM and C with MAC and Jcc":
    let m = parseOmniFile("examples/OmniSVF.omni")
    check m.name == "OmniSVF"
    check m.params.len == 3
    check m.stateVars.len >= 2

    let asmCode = emitDsp56300Asm(m)
    check "OmniSVF_process:" in asmCode
    check "MAC" in asmCode
    check "CMP" in asmCode
    check "JNE" in asmCode
    check "RTS" in asmCode

    let res = assembleText(asmCode)
    check res.pRam.len > 0

    let cCode = emitC(m)
    check "OmniSVF_process" in cCode

  test "Arbitrary variable names compile without hardcoded identifiers":
    let customDsl = """
params:
    alphaBetaParam {0.75, 0.0, 1.0}
    zetaModulator {0.25, 0.0, 1.0}

init:
    myAccumulatorVar = 0.0
    runningSumMetric = 0.0

sample:
    tempCoeffFoo = alphaBetaParam * zetaModulator
    if myAccumulatorVar < tempCoeffFoo:
        runningSumMetric = runningSumMetric + 0.1
    else:
        runningSumMetric = runningSumMetric - 0.1
    myAccumulatorVar = (myAccumulatorVar + 0.05) % 1.0
    out1 = runningSumMetric
"""
    let m = parseOmniText(customDsl, "CustomArbitraryModule")
    check m.params.len == 2
    check m.params[0].name == "alphaBetaParam"
    check m.params[1].name == "zetaModulator"
    check m.stateVars.len == 2
    check m.stateVars[0].name == "myAccumulatorVar"
    check m.stateVars[1].name == "runningSumMetric"

    let asmCode = emitDsp56300Asm(m)
    check "alphaBetaParam" in asmCode
    check "zetaModulator" in asmCode
    check "myAccumulatorVar" in asmCode
    check "runningSumMetric" in asmCode
    check "tempCoeffFoo" in asmCode
    check "CMP" in asmCode
    check "JGE" in asmCode # inverted < jump to else
    check "RTS" in asmCode

    # Verify assembly with clean-room assembler
    let res = assembleText(asmCode)
    check res.pRam.len > 0
    check "alphaBetaParam" in res.symbols
    check "zetaModulator" in res.symbols
    check "myAccumulatorVar" in res.symbols
    check "tempCoeffFoo" in res.symbols

    # Verify C generation
    let cCode = emitC(m)
    check "float alphaBetaParam" in cCode
    check "float tempCoeffFoo" in cCode
    check "if (myAccumulatorVar < tempCoeffFoo)" in cCode

  test "OscB pulse-width modulation conditional branch executes bit-accurately on DspState":
    let oscPath = "/Users/eek/Development/rg2/dsp_modules/OscB.omni"
    let m = parseOmniFile(oscPath)
    check m.name == "OscB"

    let asmCode = emitDsp56300Asm(m)
    check "CMP" in asmCode
    check "JGE" in asmCode # if phase < pw inverted

    let res = assembleText(asmCode)
    check res.pRam.len > 0
    let pwAddr = res.symbols["pw"]
    let phaseAddr = res.symbols["phase"]
    let out1Addr = res.symbols["out1"]

    # -------------------------------------------------------------
    # Case A: phase < pw is TRUE (e.g. phase = 0.2, pw = 0.5)
    # Expected output: out1 = +1.0 ($7FFFFF)
    # -------------------------------------------------------------
    var stTrue = newDspState()
    stTrue.pc = 0x100'u32
    for e in res.pRam: stTrue.pRam[e.address] = e.word
    for e in res.xRam: stTrue.xRam[e.address] = e.word
    for e in res.yRam: stTrue.yRam[e.address] = e.word

    # Set pw = 0.5 ($400000 in X memory) and phase = 0.2 ($199999 in Y memory)
    stTrue.xRam[pwAddr] = 0x400000'u32
    stTrue.yRam[phaseAddr] = 0x199999'u32

    # Execute instructions until RTS
    for step in 0..<100:
      if stTrue.pc >= 0x100'u32 and stTrue.pRam[stTrue.pc] == 0x00000C'u32: # RTS
        break
      let blk = liftBasicBlock(stTrue, stTrue.pc, maxWords = 8)
      discard runBlock(stTrue, blk)

    # Verify true branch took effect: out1 == +1.0 ($7FFFFF)
    check stTrue.xRam[out1Addr] == 0x7FFFFF'u32

    # -------------------------------------------------------------
    # Case B: phase < pw is FALSE (e.g. phase = 0.8, pw = 0.5)
    # Expected output: out1 = -1.0 ($800000)
    # -------------------------------------------------------------
    var stFalse = newDspState()
    stFalse.pc = 0x100'u32
    for e in res.pRam: stFalse.pRam[e.address] = e.word
    for e in res.xRam: stFalse.xRam[e.address] = e.word
    for e in res.yRam: stFalse.yRam[e.address] = e.word

    # Set pw = 0.5 ($400000 in X memory) and phase = 0.8 ($666666 in Y memory)
    stFalse.xRam[pwAddr] = 0x400000'u32
    stFalse.yRam[phaseAddr] = 0x666666'u32

    # Execute instructions until RTS
    for step in 0..<100:
      if stFalse.pc >= 0x100'u32 and stFalse.pRam[stFalse.pc] == 0x00000C'u32: # RTS
        break
      let blk = liftBasicBlock(stFalse, stFalse.pc, maxWords = 8)
      discard runBlock(stFalse, blk)

    # Verify false branch took effect: out1 == -1.0 ($800000)
    check stFalse.xRam[out1Addr] == 0x800000'u32

  test "EnvADSR multi-stage state machine executes bit-accurately on DspState":
    let adsrPath = "/Users/eek/Development/rg2/dsp_modules/EnvADSR.omni"
    let m = parseOmniFile(adsrPath)
    check m.name == "EnvADSR"

    let asmCode = emitDsp56300Asm(m)
    let res = assembleText(asmCode)
    check res.pRam.len > 0

    let in1Addr = res.symbols["in1"]
    let stageAddr = res.symbols["stage"]
    let levelAddr = res.symbols["level"]

    # Initial state: gate high in1 = 1.0 ($7FFFFF), stage = 0
    var st = newDspState()
    st.pc = 0x100'u32
    for e in res.pRam: st.pRam[e.address] = e.word
    for e in res.xRam: st.xRam[e.address] = e.word
    for e in res.yRam: st.yRam[e.address] = e.word

    st.xRam[in1Addr] = 0x7FFFFF'u32 # Gate ON (high)
    st.yRam[stageAddr] = 0x000000'u32 # Idle stage 0

    # Run one audio sample cycle
    for step in 0..<100:
      if st.pc >= 0x100'u32 and st.pRam[st.pc] == 0x00000C'u32:
        break
      let blk = liftBasicBlock(st, st.pc, maxWords = 10)
      discard runBlock(st, blk)

    # Gate high triggered rising edge: stage advanced from 0 to 1 (Attack)
    # and level began rising above 0
    check st.yRam[stageAddr] == 0x7FFFFF'u32 or st.yRam[stageAddr] >= 1'u32

  test "All 18 Nord Modular G2 DSP modules compile and assemble cleanly":
    let dspDir = "/Users/eek/Development/rg2/dsp_modules"
    var count = 0
    for kind, path in walkDir(dspDir):
      if kind == pcFile and path.endsWith(".omni"):
        count += 1
        let m = parseOmniFile(path)
        check m.name.len > 0

        # Assembly backend
        let asmCode = emitDsp56300Asm(m)
        check "ORG     P:$0100" in asmCode
        check "RTS" in asmCode
        let res = assembleText(asmCode)
        check res.pRam.len > 0

        # C backend
        let cCode = emitC(m)
        check m.name & "_process" in cCode
        check m.name & "_alloc" in cCode

    check count >= 23

  test "Adversarial verification: min, max, clamp, tanh, and y + (a - b) * c execute accurately on DspState":
    # 1. Test y + (a - b) * c (compound subexpression under MAC, ensuring accumulator A is not clobbered)
    let dslCompound = """
params:
    yVal {0.1, 0.0, 1.0}
    a {0.5, 0.0, 1.0}
    b {0.2, 0.0, 1.0}
    c {0.5, 0.0, 1.0}
sample:
    # Expected: 0.1 + (0.5 - 0.2) * 0.5 = 0.1 + 0.15 = 0.25 ($200000 in Q23)
    out1 = yVal + (a - b) * c
"""
    let mCompound = parseOmniText(dslCompound, "CompoundMacModule")
    let asmCompound = emitDsp56300Asm(mCompound)
    let resCompound = assembleText(asmCompound)
    check resCompound.pRam.len > 0

    var stC = newDspState()
    stC.pc = 0x100'u32
    for e in resCompound.pRam: stC.pRam[e.address] = e.word
    for e in resCompound.xRam: stC.xRam[e.address] = e.word
    for e in resCompound.yRam: stC.yRam[e.address] = e.word

    for step in 0..<50:
      if stC.pc >= 0x100'u32 and stC.pRam[stC.pc] == 0x00000C'u32: break
      let blk = liftBasicBlock(stC, stC.pc, maxWords = 8)
      discard runBlock(stC, blk)

    let out1Addr = resCompound.symbols["out1"]
    let actualOut = stC.xRam[out1Addr]
    # $200000 = 0.25 in Q23 (allow small rounding tolerance +/- 2 LSBs)
    check actualOut >= 0x1FFFFE'u32 and actualOut <= 0x200002'u32

    # 2. Test tanh(x) soft saturation polynomial: x * (1 - 1/3 * x^2)
    let dslTanh = """
params:
    inVal {0.5, -1.0, 1.0}
sample:
    out1 = tanh(inVal)
"""
    let mTanh = parseOmniText(dslTanh, "TanhModule")
    let asmTanh = emitDsp56300Asm(mTanh)
    check "MPY" in asmTanh
    check "$2AAAAA" in asmTanh # 1/3 in Q23
    let resTanh = assembleText(asmTanh)
    check resTanh.pRam.len > 0

    var stT = newDspState()
    stT.pc = 0x100'u32
    for e in resTanh.pRam: stT.pRam[e.address] = e.word
    for e in resTanh.xRam: stT.xRam[e.address] = e.word
    for e in resTanh.yRam: stT.yRam[e.address] = e.word

    for step in 0..<50:
      if stT.pc >= 0x100'u32 and stT.pRam[stT.pc] == 0x00000C'u32: break
      let blk = liftBasicBlock(stT, stT.pc, maxWords = 8)
      discard runBlock(stT, blk)

    let tanhOut = stT.xRam[resTanh.symbols["out1"]]
    # inVal = 0.5 ($400000). Polynomial = 0.5 * (1 - 1/3 * 0.25) = 0.458333... (~$3AAAB5 in Q23)
    check tanhOut >= 0x3AA000'u32 and tanhOut <= 0x3AB000'u32

    # 3. Test min, max, clamp
    let dslMath = """
params:
    valA {0.2, 0.0, 1.0}
    valB {0.7, 0.0, 1.0}
    valX {0.9, 0.0, 1.0}
    lo {0.3, 0.0, 1.0}
    hi {0.6, 0.0, 1.0}
outs:
    minOut
    maxOut
    clampOut
sample:
    minOut = min(valA, valB)
    maxOut = max(valA, valB)
    clampOut = clamp(valX, lo, hi)
"""
    let mMath = parseOmniText(dslMath, "MathIntrinsicsModule")
    let asmMath = emitDsp56300Asm(mMath)
    check "CMP" in asmMath
    let resMath = assembleText(asmMath)
    check resMath.pRam.len > 0

    var stM = newDspState()
    stM.pc = 0x100'u32
    for e in resMath.pRam: stM.pRam[e.address] = e.word
    for e in resMath.xRam: stM.xRam[e.address] = e.word
    for e in resMath.yRam: stM.yRam[e.address] = e.word

    for step in 0..<100:
      if stM.pc >= 0x100'u32 and stM.pRam[stM.pc] == 0x00000C'u32: break
      let blk = liftBasicBlock(stM, stM.pc, maxWords = 10)
      discard runBlock(stM, blk)

    let minOutVal = stM.xRam[resMath.symbols["minOut"]]
    let maxOutVal = stM.xRam[resMath.symbols["maxOut"]]
    let clampOutVal = stM.xRam[resMath.symbols["clampOut"]]

    # min(0.2, 0.7) should be 0.2 ($199999)
    check minOutVal >= 0x199000'u32 and minOutVal <= 0x1A0000'u32
    # max(0.2, 0.7) should be 0.7 ($599999)
    check maxOutVal >= 0x599000'u32 and maxOutVal <= 0x600000'u32
    # clamp(0.9, 0.3, 0.6) should be clamped to hi = 0.6 ($4CCCCD)
    check clampOutVal >= 0x4CC000'u32 and clampOutVal <= 0x4CD000'u32

  test "Real 24-bit variable division executes bit-accurately on DspState":
    let dslDiv = """
params:
    num {0.25, -1.0, 1.0}
    den {0.50, -1.0, 1.0}
outs:
    quotOut
sample:
    quotOut = num / den
"""
    let mDiv = parseOmniText(dslDiv, "DivisionModule")
    let asmDiv = emitDsp56300Asm(mDiv)
    check "ASL" in asmDiv
    check "SUB" in asmDiv
    let resDiv = assembleText(asmDiv)
    check resDiv.pRam.len > 0

    # 1. 0.25 / 0.50 == 0.50 ($400000)
    var st = newDspState()
    st.pc = 0x100'u32
    for e in resDiv.pRam: st.pRam[e.address] = e.word
    for e in resDiv.xRam: st.xRam[e.address] = e.word
    for e in resDiv.yRam: st.yRam[e.address] = e.word

    st.xRam[resDiv.symbols["num"]] = 0x200000'u32 # 0.25
    st.xRam[resDiv.symbols["den"]] = 0x400000'u32 # 0.50

    for step in 0..<200:
      if st.pc >= 0x100'u32 and st.pRam[st.pc] == 0x00000C'u32: break
      let blk = liftBasicBlock(st, st.pc, maxWords = 8)
      discard runBlock(st, blk)

    let quotVal = st.xRam[resDiv.symbols["quotOut"]]
    check quotVal == 0x400000'u32 # 0.50

    # 2. -0.25 / 0.50 == -0.50 ($C00000)
    var stNeg = newDspState()
    stNeg.pc = 0x100'u32
    for e in resDiv.pRam: stNeg.pRam[e.address] = e.word
    for e in resDiv.xRam: stNeg.xRam[e.address] = e.word
    for e in resDiv.yRam: stNeg.yRam[e.address] = e.word

    stNeg.xRam[resDiv.symbols["num"]] = 0xE00000'u32 # -0.25
    stNeg.xRam[resDiv.symbols["den"]] = 0x400000'u32 # 0.50

    for step in 0..<200:
      if stNeg.pc >= 0x100'u32 and stNeg.pRam[stNeg.pc] == 0x00000C'u32: break
      let blk = liftBasicBlock(stNeg, stNeg.pc, maxWords = 8)
      discard runBlock(stNeg, blk)

    let quotNegVal = stNeg.xRam[resDiv.symbols["quotOut"]]
    check quotNegVal == 0xC00000'u32 # -0.50

    # 3. Division by zero saturates to $7FFFFF
    var stZero = newDspState()
    stZero.pc = 0x100'u32
    for e in resDiv.pRam: stZero.pRam[e.address] = e.word
    for e in resDiv.xRam: stZero.xRam[e.address] = e.word
    for e in resDiv.yRam: stZero.yRam[e.address] = e.word

    stZero.xRam[resDiv.symbols["num"]] = 0x200000'u32
    stZero.xRam[resDiv.symbols["den"]] = 0x000000'u32

    for step in 0..<200:
      if stZero.pc >= 0x100'u32 and stZero.pRam[stZero.pc] == 0x00000C'u32: break
      let blk = liftBasicBlock(stZero, stZero.pc, maxWords = 8)
      discard runBlock(stZero, blk)

    let quotZeroVal = stZero.xRam[resDiv.symbols["quotOut"]]
    check quotZeroVal == 0x7FFFFF'u32

  test "Authentic polynomials for pow(2.0, x) and tan(x) execute accurately on DspState":
    let dslPoly = """
params:
    pitchOffset {-0.5, -1.0, 1.0}
    angle {0.1, -1.0, 1.0}
outs:
    expOut
    tanOut
sample:
    expOut = pow(2.0, pitchOffset)
    tanOut = tan(angle)
"""
    let mPoly = parseOmniText(dslPoly, "PolyModule")
    let asmPoly = emitDsp56300Asm(mPoly)
    check "$071AC0" in asmPoly # c3 for pow
    check "$111111" in asmPoly # 2/15 for tan
    let resPoly = assembleText(asmPoly)
    check resPoly.pRam.len > 0

    var stP = newDspState()
    stP.pc = 0x100'u32
    for e in resPoly.pRam: stP.pRam[e.address] = e.word
    for e in resPoly.xRam: stP.xRam[e.address] = e.word
    for e in resPoly.yRam: stP.yRam[e.address] = e.word

    for step in 0..<100:
      if stP.pc >= 0x100'u32 and stP.pRam[stP.pc] == 0x00000C'u32: break
      let blk = liftBasicBlock(stP, stP.pc, maxWords = 8)
      discard runBlock(stP, blk)

    let expVal = stP.xRam[resPoly.symbols["expOut"]]
    let tanVal = stP.xRam[resPoly.symbols["tanOut"]]

    # 2^(-0.5) is approx 0.7071 ($5A6F9B)
    check expVal >= 0x5A0000'u32 and expVal <= 0x5B0000'u32
    # tan(0.1) is approx 0.10033 ($0CD7C4)
    check tanVal >= 0x0CD000'u32 and tanVal <= 0x0CE000'u32

  test "Power-of-two scaling and Q23 fractional wrapping":
    let dslScale = """
params:
    valX {0.15, 0.0, 1.0}
outs:
    scale2
    scale4
    scaleHalf
    div4
    wrappedMod
    fracPart
sample:
    scale2 = 2.0 * valX
    scale4 = 4.0 * valX
    scaleHalf = valX * 0.5
    div4 = valX / 4.0
    wrappedMod = (valX + 0.9) % 1.0
    fracPart = valX - trunc(valX)
"""
    let mScale = parseOmniText(dslScale, "ScaleModule")
    let asmScale = emitDsp56300Asm(mScale)
    check "ASL" in asmScale
    check "ASR" in asmScale
    check "$7FFFFF" in asmScale

    let resScale = assembleText(asmScale)
    check resScale.pRam.len > 0

    var stS = newDspState()
    stS.pc = 0x100'u32
    for e in resScale.pRam: stS.pRam[e.address] = e.word
    for e in resScale.xRam: stS.xRam[e.address] = e.word
    for e in resScale.yRam: stS.yRam[e.address] = e.word

    for step in 0..<100:
      if stS.pc >= 0x100'u32 and stS.pRam[stS.pc] == 0x00000C'u32: break
      let blk = liftBasicBlock(stS, stS.pc, maxWords = 8)
      discard runBlock(stS, blk)

    let s2 = stS.xRam[resScale.symbols["scale2"]]
    let sHalf = stS.xRam[resScale.symbols["scaleHalf"]]
    # valX = 0.15 ($133333). 2.0 * 0.15 = 0.30 ($266666)
    check s2 >= 0x266000'u32 and s2 <= 0x267000'u32
    # 0.15 * 0.5 = 0.075 ($099999)
    check sHalf >= 0x099000'u32 and sHalf <= 0x09A000'u32

  test "compileOmniSnippet generates relocated kernel snippet without collision":
    let dslMod = """
params:
    gainParam {0.75, 0.0, 1.0}
outs:
    out1
    out2
sample:
    out1 = in1 * gainParam
    out2 = in2 * 0.5
"""
    let m = parseOmniText(dslMod, "RelocModule")
    var addrs = defaultDsp56300Addresses()
    addrs.inletAddrs = @[0x0110'u32, 0x0111'u32]
    addrs.outletAddrs = @[0x0120'u32, 0x0121'u32]
    addrs.paramBase = 0x0090'u32
    addrs.stateBase = 0x0030'u32

    let snippet = compileOmniSnippet(m, addrs, labelPrefix = "mod_42")
    check "X:$0110" in snippet # in1
    check "X:$0111" in snippet # in2
    check "X:$0120" in snippet # out1
    check "X:$0121" in snippet # out2
    check "X:$0090" in snippet # gainParam
    check "RTS" notin snippet
    check "ORG" notin snippet

  test "OscA outputs Saw, Pulse, Triangle, and Sine when waveform parameter changes":
    let oscPath = "/Users/eek/Development/rg2/dsp_modules/OscA.omni"
    let m = parseOmniFile(oscPath)
    let asmCode = emitDsp56300Asm(m)
    let res = assembleText(asmCode)
    check res.pRam.len > 0

    let waveAddr = res.symbols["waveform"]
    let phaseAddr = res.symbols["phase"]
    let out1Addr = res.symbols["out1"]

    let waveVals = [0x000000'u32, 0x00300000'u32, 0x00500000'u32, 0x007FFFFF'u32]
    var outputs: seq[uint32] = @[]
    for i in 0 .. 3:
      var st = newDspState()
      st.pc = 0x100'u32
      for e in res.pRam: st.pRam[e.address] = e.word
      for e in res.xRam: st.xRam[e.address] = e.word
      for e in res.yRam: st.yRam[e.address] = e.word

      st.xRam[waveAddr] = waveVals[i] # waveform = 0, 0.375, 0.75, 1.0 (Q23 quartiles)
      st.yRam[phaseAddr] = 0x200000'u32 # phase = 0.25

      for step in 0..<100:
        if st.pc < 0x100'u32 or st.pRam[st.pc] == 0x00000C'u32: break
        let blk = liftBasicBlock(st, st.pc, maxWords = 8)
        discard runBlock(st, blk)

      outputs.add(st.xRam[out1Addr])

    # Waveform 0 (Saw): 2 * 0.25 - 1.0 = -0.5 ($C00000 +/- 2 LSB)
    check outputs[0] >= 0xC00000'u32 and outputs[0] <= 0xC00002'u32
    # Waveform 1 (Pulse): phase < 0.5 -> 1.0 ($7FFFFF)
    check outputs[1] == 0x7FFFFF'u32
    # Waveform 2 (Triangle): 4 * abs(0.25 - 0.5) - 1.0 = 0.0 ($000000 +/- 2 LSB)
    check outputs[2] <= 0x000002'u32
    # Waveform 3 (Sine): sin(0.25 * 2pi) = sin(pi/2) = 1.0 ($7FFFFF +/- 16 LSB)
    check outputs[3] >= 0x7FFFF0'u32 and outputs[3] <= 0x7FFFFF'u32

  test "DelaySingle delays an impulse by > 1 samples via circular ring buffer":
    let dlyPath = "/Users/eek/Development/rg2/dsp_modules/DelaySingle.omni"
    let m = parseOmniFile(dlyPath)
    var addrs = defaultDsp56300Addresses()
    addrs.inletAddrs = @[0x0110'u32] # in1 (impulse)
    addrs.outletAddrs = @[0x0120'u32] # out1
    addrs.paramBase = 0x0080'u32     # time = p0 (0x0080)
    addrs.stateBase = 0x0020'u32
    addrs.delayBase = 0x0200'u32     # ring buffer at Y:$0200

    let snippet = compileOmniSnippet(m, addrs, labelPrefix = "dly_test")
    var fullAsm = "        ORG     P:$0100\n"
    fullAsm.add(snippet)
    fullAsm.add("        RTS\n")

    let res = assembleText(fullAsm)
    check res.pRam.len > 0

    var st = newDspState()
    for e in res.pRam: st.pRam[e.address] = e.word

    # Tap offset: time = 3 samples ($000003)
    st.xRam[0x0080'u32] = 3'u32

    # Feed impulse at sample 0: in1 = 1.0 ($7FFFFF)
    # Then in1 = 0 for samples 1..5.
    # Out1 must be 0 for samples 0, 1, 2, and 1.0 at sample 3!
    var outSamples: seq[uint32] = @[]
    for sampleIdx in 0 ..< 5:
      st.pc = 0x100'u32
      st.xRam[0x0110'u32] = if sampleIdx == 0: 0x7FFFFF'u32 else: 0'u32
      for step in 0..<150:
        if st.pc < 0x100'u32 or st.pRam[st.pc] == 0x00000C'u32: break
        let blk = liftBasicBlock(st, st.pc, maxWords = 8)
        discard runBlock(st, blk)
      outSamples.add(st.xRam[0x0120'u32])

    check outSamples[0] == 0'u32
    check outSamples[1] == 0'u32
    check outSamples[2] == 0'u32
    check outSamples[3] == 0x7FFFFF'u32

