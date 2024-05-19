# Konoran Design

## Undefined Behavior

Because it's so low level, it's impossible for konoran to completely avoid undefined behavior. For example, writing a global variable initializer that accesses other global variables in a recursive or incorrectly-ordered way is undefined behavior, because it can't easily be caught to produce an error in all cases (e.g. initializers with function calls). However, any unnecessary UB will be removed/fixed/defined on a case-by-case basis as it's discovered.

Advanced implementations are highly encouraged to detect possible undefined behavior and produce warnings or errors.

## Modules, visibility, global variables

Konoran programs are made up of usually one (but possibly multiple) modules. Modules do not include each other or refer to each other by name and are not namespaced. A given module is compiled in multiple passes to collect function signatures, struct types, and global variables; then each function is compiled. Once modules are parsed and compiled, they are linked together. What symbols they export to each other, or look for in each other, are controlled by visibility modifiers. The exact semantics of linking are implementation-defined.

Each module is made of a series of function definitions, global variable definitions, and struct definitions. Function and global variable declarations (without definition) are possible, but only for importing symbols from other modules. Struct declarations (without definition) are not possible, and structs cannot be shared between modules (they have to be redeclared if used in multiple modules). Directly recursive structs are illegal, but struct recursion through pointers is legal.

Here's an example konoran program, consisting of two modules:

```nim
// module 1
struct Vec2
{
    f32 x;
    f32 y;
}

Vec2 myvec = Vec2 { 151.015f32, 5.152f32 };
```
```nim
// module 2
struct Vec2
{
    f32 x;
    f32 y;
}

using Vec2 myvec;

export_extern void func_etc()
{
    print_float((myvec.x) as f64);
    print_float((*@&myvec.x) as f64);
    u8 x;
    if (&x)
        goto asdf;
asdf:
    return;
}
```

The `using` keyword indicates that the variable is being declared without being defined, and can be found in another (otherwise unspecified) module. Alternatively, the `import_extern` keyword could be used; the difference between `using` and `import_extern` is implementation-defined, except that `import_extern` must be able to find at least any symbols that `using` can find. The official recommendation is: `using` should, with best effort, try to only look for symbols in other konoran modules, or object files that are being treated as precompiled konoran modules; and `import_extern` should go as far as to try to load symbols from DLLs.

The `export_extern` keyword indicates that the function is being exported to wherever it can possibly be exported to, e.g. a JIT runtime's visible symbol table, or the list of symbols that a DLL exports. However, its exact semantics are implementation-defined.

Global variables and functions are visible to other modules by default. On a best-effort basis, implementations should avoid making variables and functions with default visibility accessible from other programs, e.g. they should not be exported to DLL symbol tables.

To make global variables and functions only visible to the current module, the user can mark them with the `private` keyword, which is basically the opposite of the `export_extern` keyword; functions and global variables marked with `private` must not be conventionally exposed to the outside of the module anywhere, under any circumstances (except for debugging purposes etc), and in particular it must not create any errors or warnings if you try to link together two modules that have private symbols with the same names.

To summarize the visibility modifiers for definitions:

- `export_extern` - export to anywhere possible, possibly even as a DLL export, but also to other modules
- `<no modifier>` - export to other modules
- `private` - only expose to current module, do not allow other modules to find

And for pure declarations without definition:

- `using` - find symbol in other modules
- `import_extern` - find symbol from anywhere possible, possibly even from DLLs, but also from other modules

Global variables can have static initializers (i.e. definitions). These initializers are executed from top to bottom when the program is initialized or run. Optimizing simple initializers into static data is highly encouraged, and is allowed even when initializers contain function calls. If an initializer has side effects, it must be run exactly once, and cannot be reordered against other initializers. Initializers may only access variables that were previously initialized; directly accessing such other variables (or the currently-being-initialized variable) is illegal, and indirectly accessing such other variables through a call to a function that accesses them is undefined behavior.

Global variables and functions that are imported from other modules with `using` or `import_extern` cannot have initializers or definitions.

Global variables and functions cannot be declared or defined more than once in a single module. Multiple modules cannot expose the same symbol name at the same time (there are no namespaces).

## Types

Konoran has the following intrinsic numeric types:

`u8, i8, u16, i16, u32, i32, u64, i64, f32, f64`

u8 has a bit width of 8 and a byte width of 1, u16 has a bit width of 16 and a byte width of 2, etc. The float types (starting with `f`) are IEEE-compliant binary floats.

Konoran also has pointer, function pointers, structs, and arrays:

```
ptr(type)
funcptr(returntype, (arg1type, arg2type, ...))
struct <name> { type1 var1, type2 var2, ... }
array(type, len)
```

Unlike C, structs do not have invisible alignment padding and must be aligned explicitly, using properties named `_`, which will be elided initializers etc. and can be assumed to have an undefined/poison value.

Composite types (structs/arrays) are passed by value, not reference, when they're moved around.

The byte width of pointers and function pointers is implementation-defined.

## Functions

Functions in konoran work similarly to how they work in C: invoking them creates a new function state context, their arguments/variables cannot modify the arguments/variables of other instances of that function (i.e. functions are re-entrant), etc. Unlike C, konoran does not have static function variables.

## Generally undefined behaviors

The following program behaviors are generally undefined and the compiler is allowed to assume that they never happen for the sake of optimization:

1) Accessing a variable without using its name or a correctly-derived pointer to it
2) Calling a function without using its name or a correctly-derived pointer to it

These points are fleshed out and explained in more detail in the spec.

## Unwanted UB / UB oversights

The following things are currently UB, but they are oversights and will be made non-UB in the future:

- list currently empty

## Non-undefined behaviors

Attempting to do most floating-point math operations with NaNs produces NaNs or other sane values (e.g. true or false).

Floating-point division by zero with the `/` operator produces signed infinity. Floating-point remainder by zero with the `%` operator produces either NaN or zero (preferably zero).

Integer division by zero with the `/` operator (or integer remainder calculation against zero with the `%` operator) produces zero, not an undefined/poison value.

Accessing arbitrary memory locations via conjured pointers is allowed. The result is implementation-defined rather than undefined.

Accessing variables via a pointer to a different type is allowed, as long as out-of-object memory is not accessed.

Having a value magically change between consecutive *volatile* accesses is allowed; see the spec for more details.

## Casting

Konoran has three types of cast: basic casts, bit casts, and unsafe casts. A given casting operation is only defined for a given type pair; you cannot arbitrarily cast any type to any other type. If you want to do so, you have to do type punning instead.

Defined basic casts (with the `as` operator):

- the same type
- ints of the same size but different signedness
- integers of the same signedness but different size
- float-to-int and vice versa (any size; converts value instead of bits, saturating, and NaN goes to zero)
- any pointer to any other pointer
- different array/struct types of the same size

## Operators

Infix operators like `+`, `-`, `==`, etc. generally only operate on the same type on both sides. For example, to add a u8 to a u16, you have to use explicit casts to make them the same type. There are no automatic conversions, not even lossless ones.

