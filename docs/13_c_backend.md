---
layout: page
title: C Backend
---

# C Backend

Omni features a clean-room C backend that translates Omni DSP modules directly into clean, self-contained, dependency-free C99/C11 source code (`.c`).

The emitted C code is fully portable across operating systems, architectures, and embedded environments. It is ideal for:
- Integrating Omni algorithms directly into C and C++ audio plugins (JUCE, CLAP, VST3, AU).
- Compiling to WebAssembly (WASM) for high-performance in-browser synthesis via the Web Audio API AudioWorklet.
- Flashing onto embedded microcontrollers and DSP boards (such as ARM Cortex-M, ESP32, and Daisy) that provide a standard C compiler.

---

## Compiling to C

To emit standalone C code from an Omni source file, pass the `--target:c` flag (or the shorthand `--compileOnly` / `-c`):

```bash
# Generate clean C99/C11 code:
omni OmniSine.omni --target:c

# Specify an output directory or file name:
omni OmniSine.omni --target:c -o:./build/ -n:MySine.c
```

This generates a standalone `.c` file containing all the structs, allocation logic, and DSP loop functions needed to run the module.

---

## Architecture & Code Structure

The generated C code is organized into clear functional layers:

### 1. Struct Definitions & Memory State
Every Omni module is encapsulated in a dedicated C struct containing all persistent state variables, inputs, outputs, and internal scratch buffers:

```c
typedef struct {
    float phase;
    float freq;
    float in1;
    float out1;
} OmniSine_t;
```

### 2. Lifecycle & Initialization Functions
Standard C lifecycle routines are emitted for allocating and initializing the DSP object:

```c
// Allocate and initialize module instance
OmniSine_t* OmniSine_new(float samplerate, int bufsize);

// Initialize or reset existing state
void OmniSine_init(OmniSine_t* self, float samplerate, int bufsize);

// Free allocated memory
void OmniSine_free(OmniSine_t* self);
```

### 3. Processing Loops (`perform` and `sample`)
The processing routines execute audio blocks with zero overhead:

```c
// Sample-by-sample processing
void OmniSine_sample(OmniSine_t* self);

// Buffer processing block
void OmniSine_perform(OmniSine_t* self, const float** ins, float** outs, int num_samples);
```

---

## Floating-Point Precision

By default, the C backend operates with standard IEEE 754 32-bit single-precision floating point (`float`). You can also configure buffer precision using the `-b` / `--performBits` option:

```bash
# 32-bit floating point buffers (default)
omni OmniSine.omni --target:c --performBits:32

# 64-bit double precision buffers
omni OmniSine.omni --target:c --performBits:64
```

---

## Embedded & WASM Integration

Because the emitted C code has **zero runtime dependencies on Nim, garbage collectors, or external audio libraries**, it can be compiled directly with any C compiler:

```bash
# Compile to a static object with GCC or Clang:
clang -O3 -c OmniSine.c -o OmniSine.o

# Compile to WebAssembly with Emscripten:
emcc -O3 -s STANDALONE_WASM OmniSine.c -o OmniSine.wasm
```

<br>

## [Next: 14 - Motorola DSP56300 Backend](14_dsp56300.md)
