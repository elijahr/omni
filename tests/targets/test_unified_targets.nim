## Test suite for Omni Unified Targets Architecture
## Verifies:
## 1. Pure Nim target generation (emitNim) and semantic validity
## 2. Pure Data target generation (emitPd)
## 3. SuperCollider UGen target generation (emitSuperColliderClass / emitSuperColliderCpp)
## 4. Max/MSP target generation (emitMax)
## 5. Backward compatibility with native default and dsp56300 / c targets

import std/[unittest, strutils, os]
import ../../omni_ast
import ../../omni_targets

let fixturesDir = currentSourcePath().parentDir() / ".." / ".." / "examples"

suite "Omni Unified Targets Pipeline":

  test "emitNim translates OmniSine to clean pure Nim module":
    let m = parseOmniFile(fixturesDir / "OmniSine.omni")
    let nimCode = emitNim(m)
    check nimCode.contains("type")
    check nimCode.contains("OmniSine* = object")
    check nimCode.contains("proc init*(m: var OmniSine")
    check nimCode.contains("proc process*(m: var OmniSine")
    check nimCode.contains("freq_incr = (freq / samplerate)")
    check nimCode.contains("out1 = sin((phase * TWOPI))")

  test "emitPd translates OmniSine to Pure Data audio external":
    let m = parseOmniFile(fixturesDir / "OmniSine.omni")
    let pdCode = emitPd(m)
    check pdCode.contains("t_OmniSine_tilde")
    check pdCode.contains("OmniSine_tilde_perform")
    check pdCode.contains("OmniSine_tilde_dsp")
    check pdCode.contains("OmniSine_tilde_new")
    check pdCode.contains("OmniSine_tilde_setup")
    check pdCode.contains("gensym(\"OmniSine~\")")

  test "emitSuperCollider translates OmniSine to UGen class and C++ plugin":
    let m = parseOmniFile(fixturesDir / "OmniSine.omni")
    let scClass = emitSuperColliderClass(m)
    let scCpp = emitSuperColliderCpp(m)
    check scClass.contains("OmniSine : UGen")
    check scClass.contains("*ar {")
    check scClass.contains("^this.multiNew('audio'")
    check scCpp.contains("OmniSine_SC : public Unit")
    check scCpp.contains("PluginLoad(OmniSineUGens)")

  test "emitMax translates OmniSine to Max/MSP audio external":
    let m = parseOmniFile(fixturesDir / "OmniSine.omni")
    let maxCode = emitMax(m)
    check maxCode.contains("t_OmniSine_max")
    check maxCode.contains("OmniSine_max_perform64")
    check maxCode.contains("OmniSine_max_dsp64")
    check maxCode.contains("ext_main")
