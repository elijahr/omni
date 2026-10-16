<a name="logo" href = "https://vitreo12.github.io/omni">
    <div align="center">
        <img src="omni_logo_text_transparent.png" alt="Omni logo" width="30%" height="30%">
    </div>
</a>

![Build Status (master)](https://github.com/vitreo12/omni/actions/workflows/ci.yml/badge.svg?branch=master)

<a href="https://www.buymeacoffee.com/vitreo12" target="_blank"><img src="https://cdn.buymeacoffee.com/buttons/default-orange.png" alt="Buy Me A Coffee" height="41" width="174"></a>

Omni is a cross-platform DSL (Domain Specific Language) for low level audio programming. 
It aims to be a new, expressive and easy to use programming language to code audio algorithms in.

Omni leverages Nim and C to compile code to self-contained static or shared libraries that can then be loaded and used anywhere. So far, two wrappers have already been written to compile omni code to [SuperCollider](https://supercollider.github.io/) UGens ([omnicollider](https://github.com/vitreo12/omnicollider)), or [Max 8](https://cycling74.com/) objects ([omnimax](https://github.com/vitreo12/omnimax)).

In addition, Omni supports clean standalone C99/C11 code generation (`--target:c`) and Motorola DSP56300 / DSP56311 24-bit fixed-point assembly generation (`--target:dsp56300`, `--target:dsp56300Asm`, and the dedicated `omnidsp` compiler).

A syntax highlighting [VSCode](https://code.visualstudio.com/) plugin is available by looking for [omni](https://github.com/vitreo12/vscode-omni) in the Extensions Marketplace.

## **Requirements**

1) [nim](https://nim-lang.org/) (Omni supports Nim 1.6 LTS, 2.0 LTS, 2.2 stable, and devel)
2) [git](https://git-scm.com/)
3) A C compiler (GCC, Clang, or MSVC)

## **Installation**

To install `omni`, simply use the `nimble` package manager (bundled with Nim):

    nimble install omni -y

This installs both the `omni` compiler and the dedicated `omnidsp` Motorola DSP56300 compiler to `~/.nimble/bin`.

## **Usage**

Run `omni -h` or `omnidsp -h` to inspect all available options.

### Standard C / Native Shared Library

When compiling without backend flags, the output is either a static or shared library (depending on the `--lib` flag), accompanied by an `omni.h` header:

    omni examples/OmniSaw.omni -o:./

### Clean C99/C11 Code Generation

To emit self-contained, dependency-free C code:

    omni examples/OmniSine.omni --target:c

### Motorola DSP56300 Assembly Generation

To emit clean-room 24-bit fixed-point Motorola DSP56300 / DSP56311 assembly:

    omni examples/OmniSine.omni --target:dsp56300

Or use the dedicated `omnidsp` binary directly:

    omnidsp examples/OmniSine.omni -o:./OmniSine.asm

## **Sine oscillator example**

`Sine.omni`

```nim
ins:  1
outs: 1

init:
    phase = 0.0

sample:
    incr  = in1 / samplerate
    out1  = sin(phase * TWOPI)
    phase = (phase + incr) % 1.0
```

To compile it to a native library:

    omni Sine.omni

To compile it to DSP56300 assembly:

    omnidsp Sine.omni

## **Website & Documentation Structure**

Omni documentation is hosted at [https://vitreo12.github.io/omni/](https://vitreo12.github.io/omni/).

The site is maintained on the `gh-pages` branch using Jekyll:
- `index.md`: Main landing page and introduction.
- `_config.yml`, `_layouts/`, `_includes/`, `_sass/`: Jekyll styling and theme templates.
- `docs/`: Numbered topic guides:
  - `01_syntax.md`: Language syntax, blocks, and statements.
  - `02_ins_outs_params.md`: Inputs, outputs, and parameters specification.
  - `03_init.md`: Initialization block and lifecycle.
  - `04_perform_sample.md`: Audio processing blocks (`perform` and `sample`).
  - `05_def.md`: Function definitions and macros.
  - `06_struct.md`: Struct definitions and custom types.
  - `07_data.md`: Audio buffers and static data tables.
  - `08_buffer.md`: External buffer interfaces.
  - `09_delay.md`: Delay lines and feedback lines.
  - `10_code_composition.md`: Modular code composition and includes.
  - `11_writing_wrappers.md`: Guide to authoring third-party host wrappers.
  - `12_nim_interop.md`: Interoperability with Nim code and libraries.
