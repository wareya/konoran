# Konoran Design

## Undefined Behavior

Because it's so low level, it's impossible for konoran to completely avoid undefined behavior. For example, writing a global variable initializer that accesses other global variables in a recursive or incorrectly-ordered way is undefined behavior, because it can't easily be caught to produce an error in all cases (e.g. initializers with function calls). However, any unnecessary UB will be removed/fixed/defined on a case-by-case basis as it's discovered.

Advanced implementations are highly encouraged to detect possible undefined behavior and produce warnings or errors.

## Modules, visibility, global variables

Konoran programs are made up of usually one (but possibly multiple) modules. Modules do not include each other or refer to each other by name and are not namespaced. A given module is compiled in multiple passes to collect function signatures, struct types, and global variables; then each function is compiled. Once modules are parsed and compiled, they are linked together. What symbols they export to each other, or look for in each other, are controlled by visibility modifiers. The exact semantics of linking are implementation-defined.

Each module is made of a series of function definitions, global variable definitions, and struct definitions. Function and global variable declarations (without definition) are possible, but only for importing symbols from other modules. Struct declarations (without definition) are not possible, and structs cannot be shared between modules (they have to be redeclared if used in multiple modules). Directly recursive structs are illegal, but struct recursion through pointers is legal.

Here's an example konoran program, consisting of two modules:

```rust
// module 1
struct Vec2
{
    f32 x;
    f32 y;
}

Vec2 myvec = Vec2 { 151.015f32, 5.152f32 };
```
```rust
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

These are unsigned and signed integers (two's complement), and IEEE binary floating point numbers.

Konoran has the following pointer types:

```
ptr(type)
funcptr(returntype, (arg1type, arg2type, ...))
```

Pointers can be casted back and forth with `u64` (or whatever the target's pointer-sized int type is) using the `bit_as` operator (e.g. `(my_ptr) bit_as u64`). With regards to the values stored in this u64 after casting, the in-memory "size" of a u8 must be 1, of a u16 must be 2, etc.

Konoran has the following composite types:

```
struct <name> { type1 var1, type2 var2, ... }
array(type, len)
```

Struct types consist of a series of members (also called variables, properties, etc) declared one at a time, with no specified value. Struct values consist of a series of values with the appropriate type for the struct being constructed, and are prefixed with the name of the struct (e.g. `Vec2 { 1.0f32, 0.5f32 }`).

Structs have a fixed, known size, and their members are not reordered. Structs do not have any invisible padding; even if `u32`s need to be aligned to 4 bytes on the target platform, a struct that goes `struct ... { u8 a; u32 b; }` will have the `b` property start at the second byte of the struct, and the struct will be 5 bytes long. Alignment must be done manually. To make this easier, structs can have multiple members with the name `_`, none of which are accessible, and are assumed to have an unknown (possibly undefined/poison) value if accessed with pointer arithmetic. It should be noted that padding around floats should ideally be of a float type, and same for non-floats. For example:

```
struct asdf
{
    f32 a;
    f32 _; // padding
    u64 b;
}
struct asdf2
{
    i32 a;
    u32 _; // padding
    u64 b;
}
```

Composite types (structs/arrays) are passed by value when calling functions with them as arguments, or returning them from functions.

The konoran implementation is allowed, but not encouraged, to produce an error if structs have misaligned members for the target platform.

## Functions

Functions have return types, names, argument lists, and bodies. A function's signature consists of its return type and the list of its argument types. Function bodies consist of a series of local variable declarations/definitions, statements, labels, and branching statements (if statements and gotos), in any order. Branches can point "upwards", not just downwards. Branches can only point to labels within the current function; they cannot point to the insides of other functions. Function bodies must return in all control paths, but are allowed to have redundant returns that create dead code. The return value must be of the same type as is declared in the function's signature. `void` functions return no value.

Variable declarations (but not their definitions/assignments if they have one) are hoisted to the top of the function, but this is possibly subject to change. (This indirectly means that jumps are incapable of leaving the stack in an invalid state. If variable declarations ever stop being hoisted, branches will be forbidden from jumping to a point in the function that has a differently shaped or typed stack.) Variable declarations in functions (and their arguments) only last for the body of the function and do not affect other functions. Function-specific variable declarations are allowed to "shadow" global variables and functions; doing so must not produce an error or warning.

Variables are mutable and can be rewritten. Const variables do not exist, but this is possibly subject to change. Compilers are free to detect and mark const variables as const and optimize them away, as long as they're never explicitly accessed.

When a function begins, its arguments are treated as normal variables, with their values pre-filled. They cannot be redefined in that function's body.

## Generally undefined behaviors

The following program behaviors are generally undefined and the compiler is allowed to assume that they never happen for the sake of optimization:

1) Accessing a variable without using its name or a correctly-derived pointer to it
2) Calling a function without using its name or a correctly-derived pointer to it

Point 1 means that other code using that variable is allowed to assume that it doesn't suddenly change for no reason, even if an incorrectly-derived pointer value might be pointing to it. For example:

```rust
u32 x = 0;
ptr(u32) maybe_x = (randi()) as ptr(u32);
*maybe_x = 16u32;
print_float((x) as f64);
```

The above program contains undefined behavior if `randi()` is capable of producing a value matching the address of the `x` variable. As such, a compiler optimizing this code is allowed to assume that the value passed to `print_float()` remains `0.0f64`, even if `*maybe_x` is written to in the meantime, because even if `maybe_x` points to `x`, it was not correctly derived.

Importantly, point 1 does not make it instant UB to access/modify data via conjured pointers. The optimizer is merely allowed to assume that accesses via conjured pointers do not modify named variables. Arbitrary memory and heap memory are assumed to be disjoint from "variables".

Point 2 means that the optimizer can remove functions that are never referenced even if code might accidentally construct the value of a function pointer that would point at that function if it hadn't been removed. The same is true of point 1 and variables. However, this does not apply to `export_extern`'d global variables or functions.

The implementation is allowed to define new UB in situations where threads, OS access, or language extensions are involved. For example, it's OK for an implementation to define it to be UB to read and write to a single variable or memory location from two threads without using a synchronization primitive or memory fence, even if those accesses are direct or use correctly-derived pointers.

Implementations are allowed to specify things as being defined even if they're specified as UB here. For example, implementations are allowed to specify that it's not UB for a variable's value to magically change when a memory fence or thread synchronization operation is somehow performed.

Implementations are allowed to specify unaligned memory accesses as UB, but this is discouraged and it's strongly recommended that they specify them as implementation-defined instead.

## Unwanted UB / UB oversights

The following things are currently UB, but they are oversights and will be made non-UB in the future:

- list currently empty

## Non-undefined behaviors

Attempting to do most floating-point math operations with NaNs produces NaNs or other sane values (e.g. true or false).

Floating-point division by zero with the `/` operator produces signed infinity. Floating-point remainder by zero with the `%` operator produces either NaN or zero (preferably zero).

Integer division by zero with the `/` operator (or integer remainder calculation against zero with the `%` operator) produces zero, not an undefined/poison value.

Accessing arbitrary memory locations via conjured pointers is allowed. The result is implementation-defined rather than undefined.

Accessing variables via a pointer to a different type is allowed; for example, accessing a `f32` via a `ptr(u16)` pointing at the first or third byte of the `f32` is allowed.

Having a value magically change between consecutive *volatile* accesses is allowed, i.e. the compiler cannot change the order or number of volatile operations or what addresses they operate on. In particular, volatile writes to even a correctly-derived pointer cannot be optimized away (even if the variable the pointer points at is never used again), and volatile reads from even a correctly-derived pointer must assume that the value may have magically changed since the last time it was accessed (even if it has not been accessed).

Volatile memory acceses must be treated as though they have (non-UB-producing) side effects. In particular, optimizations must not change volatile memory accesses in order, number, or address. Implementations are allowed to specify arbitrary side-effects for volatile memory accesses. Volatile memory accesses are not typically assumed to modify variables or memory that they do not point at, but implementations are allowed to specify situations where they do. For example, an implementation might specify that doing a volatile read of a status register can cause an interrupt handler to run; normally, the implementation's optimizer is allowed to assume that this interrupt handler doesn't modify any variables or memory that the current non-interrupt code is working on, but the implementation is allowed to specify that it does, and prevent its own optimizer from assuming that those variables or memory are unchanged during the interrupt. If the implementation does not specify such a situation, then it is assumed that volatile memory accesses do not alter unrelated variables/memory, and the optimizer is free to use that assumption.

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

