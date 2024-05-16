# Konoran Spec

Work-in-progress living spec for Konoran, a low-level language designed as a compiler target, with a complexity between C and assembly.

## Programs

Konoran programs consist of a number of modules linked together, possibly dynamically, and possibly with non-Konoran modules. The specific semantics of linking are entirely implementation-defined.

Standalone konoran programs usually export a `main` function that can be executed by an OS.

Konoran programs have statically-initalized data. Sometimes this data can only be initialized by running code. In such cases, that initialization code must be run before any function in the Konoran program is called, and it must be run exactly once. This initialization code might call out to non-Konoran code, and the environment must tolerate this.

The above-described initialization code comes from each individual module. When linking multiple modules together into a single program, the exact order in which each module's initialization code is run is implementation-defined; however, implementations are strongly encouraged to allow the user to control it. For example, if you link a user's modules called `main` and `mathlib` together with the command `link main.o mathlib.o`, the implementatino could specify that the initializers from `main.o` should run before those from `mathlib.o` because it was listed first in the linking command; such behavior would comply with this encouragement.

## Modules

Konoran modules do not have a preprocessor, do not "include" or "use" specific other modules, are not logically namespaced, and can be compiled independently of one another in any order before linking.

Konoran modules consist of a series of:

- Struct type definitions
- Global variable/constant definitions
- Global variable imports
- Function definitions
- Function imports

These can come in any order, including interleaved order, and there are no pre-declarations for things that are defined in the current module. The implementation must run a pre-pass over the module to collect definitions and imports. Global variable definitions can contain arbitrary expressions and are evaluated from top to bottom on program initialization. It's strongly encouraged that advanced implementations fold logically constant initializers for global variables down into static data, but this is not mandatory. Global constant initializers *must* be folded down into static data. Global variables do not require initializers (note that without them, their initial state is undefined), but global constants do.

Within a single module, two structs cannot have the same name, two functions cannot have the same name, two variables cannot have the same name, and two constants cannot have the same name.

Structs are not exposed to other modules.