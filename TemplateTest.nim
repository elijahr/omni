#nim c --app:lib --gc:none -d:release -d:danger --noMain

import macros
import dsp_macros
import sc_types
import math

#For rt_alloc/rt_alloc0/rt_realloc/rt_free
import RTAlloc/rt_alloc

import RTAlloc/sc_data
    
ins 1:
    "freq"

outs 1:
    "sine_out"

dspTypes:
    Phasor[T : SomeFloat]:
        phase : T
    
    AnotherType[T]:
        phasor : Phasor[T]


#[ #An object like this is allocated on the stack at the UGenConstructor() function. Its content would then
#be deep copied into the UGen object... This is not the best solution. It would be better to interact directly
#with the object allocated inside of the UGen, instead of copying the contents of some other to it
type Phasor = object
    phase : float
    
expandMacros:
    constructor:
        let 
            sampleRate = 48000.0
            phasor  = Phasor(phase : 0.13)   #allocated on stack
            someData = Data(100)

        var phase = 0.0
        
        new phase, sampleRate, phasor, someData

expandMacros:
    perform:
        var 
            frequency : float
            sine_out : float

        sample:
            frequency = in1

            if phase >= 1.0:
                phase = 0.0
            
            #Can still access the var inside the object, even if named the same as another "var" declared variable (which produces a template with same name)
            phasor.phase = 2.3

            sine_out = cos(phase * 2 * PI) #phase equals to phase_var[]
            
            out1 = sine_out

            phase += abs(frequency) / (sampleRate - 1) #phase equals to phase_var[]
 ]#
#################
# TESTING SUITE #
#################
#[ 
var 
    ins_ptr_void  = alloc0(sizeof(ptr cfloat) * 2)      #float**
    in_ptr1_void = alloc0(sizeof(cfloat) * 512)         #float*

    ins_ptr_SC = cast[ptr ptr cfloat](ins_ptr_void)
    ins_ptr = cast[CFloatPtrPtr](ins_ptr_void)
    in_ptr1 = cast[CFloatPtr](in_ptr1_void)

    outs_ptr_void  = alloc0(sizeof(ptr cfloat) * 2)     #float**
    out_ptr1_void = alloc0(sizeof(cfloat) * 512)        #float*
    
    outs_ptr_SC = cast[ptr ptr cfloat](outs_ptr_void)
    outs_ptr = cast[CFloatPtrPtr](outs_ptr_void)
    out_ptr1 = cast[CFloatPtr](out_ptr1_void)

#Fill frequency input with 200hz
for i in 0 .. 511:
    in_ptr1[i] = 200.0

ins_ptr[0]  = in_ptr1
outs_ptr[0] = out_ptr1

var ugen = UGenConstructor()

UGenPerform(ugen, cast[cint](512), ins_ptr_SC, outs_ptr_SC)

dealloc(ins_ptr_void)
dealloc(in_ptr1_void)
dealloc(outs_ptr_void)
dealloc(out_ptr1_void)

UGenDestructor(ugen)
 ]#