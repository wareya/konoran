# Konoran Spec

Work-in-progress living spec for Konoran, a low-level language designed as a compiler target, with a complexity between C and assembly.

This spec is *very* early; right now, the design philosophy document ([Design.md](Design.md)) is more detailed!

## Parsing

Konoran's reference implementation uses a PEG-like recursive descent parser generator (a declarative, dynamic one) to parse the program. Implementations must use a parser that accepts and rejects the same programs and produces an equivalent syntax tree.

Token patterns are derived from the grammar (some of which are regexes, and some of which are literals) and matched against the source text in top-to-bottom order (for regexes) or in order of decreasing length (for literals/symbols; with length ties broken by top-to-bottom order) to derive a token stream. Regexes are matched first. Whitespace is used as an explicit token boundary and is stripped. Tokens cannot span lines. C-style, C++-style, and Bash-style (`#`) comments are supported, and are matched for before tokens. Then the token stream is fed to the parser.

Struct/function/constant/variable names ("struct/symbol names") are allowed to be the same as syntax keywords as long as the resulting declaration/definition is not syntactically ambiguous. The konoran grammar is designed in a way that doesn't need to know any specific struct/symbol names to be able to detect when such a name (rather than a keyword) is being used. If a usage would be visually ambiguous, the grammar determines which interpretation is chosen, regardless of whether or not a conflicting struct/symbol name has been defined.

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

Within a single module, two structs cannot have the same name, two functions cannot have the same name, and two global constants or global variables cannot have the same name.

Structs are not exposed to other modules. If two modules want to use the same struct they need to redefine it.

Functions and global variables (not global constants) can have the following visibility specifiers:

- `export_extern` - export to anywhere possible, possibly even as a DLL export, but also to other modules
- `<no modifier>` - export to other modules
- `private` - only expose to current module, do not allow other modules to find
- `using` - find symbol in other modules; local definition is illegal
- `import_extern` - find symbol from anywhere possible, possibly even from DLLs, but also from other modules; local definition is illegal

The exact semantics of `export_extern` and `import_extern` are implementation-defined, except that they must be the same as or supersets of the default or `using` visibility respectively.

The exact semantics of linking are implementation-defined.

Functions with non-private visibility are imported and exported according to the platform's C ABI, unless array or struct values pass in or out of the function. Functions that have array or struct values pass through them are not guaranteed for conform to the platform's C ABI, and may have a different implementation-defined representation. Pointers to arrays/structs are fine and must be passed according to the platform's C ABI.

## Functions

Functions have return types, names, argument lists, and bodies. A function's signature consists of its return type and the list of its argument types. Function bodies consist of a series of local variable declarations/definitions, statements, labels, and branching statements (if statements and gotos), in any order. Branches can point "upwards", not just downwards. Branches can only point to labels within the current function; they cannot point to the insides of other functions. Function bodies must return or enter an infinite loop in all control paths, but are allowed to have redundant returns that create dead code. In other words, falling through to the end of the function without a return is illegal code and must produce an error. The return value must be of the same type as is declared in the function's signature. `void` functions return no value, but must still explicitly return.

Variables are mutable and can be rewritten. Constexpr variables exist; they must be defined upon declaration and cannot be modified/rewritten. Variables and const variables follow the same logical access rules and cannot shadow each other in the same scope. Compilers are free to detect and mark const variables as const and optimize them away, as long as they're never explicitly accessed, or fold/inline them, even if their address is taken and modified. Writing to a const variable via a pointer is undefined behavior.

Jumps, including goto, are incapable of leaving the stack in an invalid state. Variable declarations in functions (and their arguments) only last for the body of the function and do not affect other functions. Function-specific variable declarations are allowed to "shadow" global variables and functions; doing so must not produce an error or warning.

When a function begins, its arguments are treated as normal variables, with their values pre-filled, in a "zeroth" scope. The function body begins the "first" scope, and can declare local variables that shadow its arguments. Other block scopes (defined in the grammar as `$statementlist$`) open new scopes, which can shadow variables created in outer scopes. Importantly, scopes do not logically grow or shrink the stack, so `goto`ing into them past variable declarations is not instant UB; shadowing and scoping are purely a name lookup concern, and the function must logically behave as though all variable declarations were invisibly hoisted to the top of the function (except for name lookup and shadowing). The implementation is highly encouraged to perform disjoint stack slot reuse optimizations to reduce how much stack space a given function uses up with variables declared in disjoint block scopes.

To clarify, if a variable is declared in an inner scope, it becomes inaccessible once that scope ends, and the optimizer is allowed to make this assumption. However, `goto`ing over a declaration is not UB, as any code that can see the variable's name is allowed to legally access it; variable declarations are not runtime stack manipulation instructions.

Performing pointer arithmetic to conjure a pointer to a local variable that is not currently in-scope is UB because conjuring pointers to variables is UB, and the optimizer is allowed to assume that a konoran program does not do this.

Unlike C, konoran does not have static function variables. Global variables must be used instead.

## Types

Konoran has the following primitive/intrinsic numeric types:

```rs
u8, i8, u16, i16, u32, i32, u64, i64, f32, f64
```

These are unsigned and signed integers (two's complement) and IEEE-compliant binary floating point numbers. u8 has a bit width of 8 and a byte width of 1, u16 has a bit width of 16 and a byte width of 2, etc.

Konoran has the following pointer types:

```rs
ptr(type)
funcptr(returntype, (arg1type, arg2type, ...))
```

Pointers can be casted back and forth with `u64` (or whatever the target's pointer-sized int type is) using the `bit_as` operator (e.g. `(my_ptr) bit_as u64`). With regards to the values stored in this u64 after casting, the in-memory "size" of a u8 must be 1, of a u16 must be 2, etc.

Konoran has the following composite types:

```rs
struct <name> { type1 var1, type2 var2, ... }
array(type, len)
```

Struct types consist of a series of members (also called variables, properties, etc) declared one at a time, with no specified value. Struct values consist of a series of values with the appropriate type for the struct being constructed, and are prefixed with the name of the struct (e.g. `Vec2 { 1.0f32, 0.5f32 }`).

Structs have a fixed, known size, and their members are not reordered. Structs do not have any invisible padding; even if `u32`s need to be aligned to 4 bytes on the target platform, a struct that goes `struct ... { u8 a; u32 b; }` will have the `b` property start at the second byte of the struct, and the struct will be 5 bytes long. Alignment must be done manually. To make this easier, structs can have multiple members with the name `_`, none of which are accessible, and are assumed to have an unknown (possibly undefined/poison) value if accessed with pointer arithmetic. It should be noted that padding around floats should ideally be of a float type, and same for non-floats. For example:

```rs
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

Array types have a size of the size of their inner type multiplied by their length. Structs have a size of the sum of the sizes of their inner types.

Type punning is allowed and legal. Casting pointers to `u64` (or, on 32-bit architectures, `u32`) and back is allowed, and is allowed to either retain or destroy provenance information.

The konoran implementation is allowed, but not encouraged, to produce an error if structs have misaligned members for the target platform.

### Volatile memory access

There is no `volatile` variable modifier. However, pointer values can be marked as having volatile access semantics using the `@` operator. So, to use an mmio register, you might use something like the following code:

```rs
ptr(u32) my_mmio_reg = (0x80000030u64) as ptr(u32);
u32 my_val = *@my_mmio_reg; // @ causes * to perform a volatile load
*@my_mmio_reg = my_val + 10u32; // @ causes = to perform a volatile store
```
Given the above context, the following would NOT perform a volatile operation:
```rs
@my_mmio_reg = (0x80000040u64) as ptr(u32); // the volatility modifier does nothing in this case; the pointer value is just replaced
```

This more closely reflects how compiler backends tend to work than a `volatile` variable modifier. As such, konoran chose this approach to exposing volatile memory operations, rather than a variable modifier.

Volatile memory acceses must be treated as though they have (non-UB-producing) side effects. In particular, optimizations must not change volatile memory accesses in order, number, or address. Implementations are allowed to specify arbitrary side-effects for volatile memory accesses. Volatile memory accesses are not typically assumed to modify variables or memory that they do not point at, but implementations are allowed to specify situations where they do. For example, an implementation might specify that doing a volatile read of a status register can cause an interrupt handler to run; normally, the implementation's optimizer is allowed to assume that this interrupt handler doesn't modify any variables or memory that the current non-interrupt code is working on, but the implementation is allowed to specify that it does, and prevent its own optimizer from assuming that those variables or memory are unchanged during the interrupt. If the implementation does not specify such a situation, then it is assumed that volatile memory accesses do not alter unrelated variables/memory, and the optimizer is free to use that assumption.

### Casts

Konoran has three casting operators: `as`, `unsafe_as`, and `bit_as`. They are defined over the following type pairs and have the given behaviors. Note that it is not possible to change both the signedness and size of an integer in a single cast operation.

```
(val) as <val type>
    Does nothing. Returns original value.

(pointer val) as <different pointer type>
    Reinterprets the pointer as pointing at a different type. Legal even if casting between function pointer and normal pointer, however the resulting pointer may be illegal to use depending on how the platform defines the behavior of such an operation.

(integer_val) as <integer type of same size>
    Converts an integer to an integer of a different signedness. Does not modify the underlying bit representation of the integer.

(integer_val) as <integer type of same signedness>
    Truncates or extends an integer to a different size. When truncating, upper bits are dropped. When extending, signed integers get sign-extended, and unsigned integers get zero-extended.

(float_val) as <different float type>
    Converts a floating-point number from one width to another. If the number doesn't have a nearby value in the target type, either positive/negative infinity or a minimal/maximal value is given (usually positive/negative infinity).

(int_val) as <float type>
    Converts an integer to a floating-point number. If the integer cannot be represented exactly, the closest value is given.

(float_val) as <integer type>
    Converts an floating-point number to an integer type. If the value is outside of the integer range, either the minimum or maximum integer value is given, whichever is closer.
```
```
(float_val) unsafe_as <integer type>
    Converts an floating-point number to an integer type. If the value is outside of the integer range, the result is undefined.
```
```
(val) bit_as <val type>
    Does nothing. Returns original value.

(pointer val) bit_as <different pointer type>
    Reinterprets the pointer as pointing at a different type. Same semantics as a normal pointer-to-pointer cast.

(pointer_val) bit_as <u64> (or u32 on 32-bit architectures)
    Converts a pointer to an integer-based representation.

(ptr_sized_int_val) bit_as <pointer type>
    Converts an integer to a pointer. When a pointer is casted to u64 (or u32 on 32-bit architectures) and then back to the same pointer type, it must point to the same object and memory location.
    If the compiler has provenance or aliasing analysis, the resulting pointer value is treated as being derived any value that contributed to the calculation of the given integer.

(float/int value) bit_as <float or int type of same size>
    Reinterprets the bits of a value of one primitive type as belonging to another type. The exact bit representation is used, not the logical value. Equivalent to type punning a pointer.
    
(struct/array value) bit_as <struct/array of same size>
(struct/array value) bit_as <primitive of same size>
(float/int value) bit_as <struct/array of same size>
    Equivalent to memcpying the given value into a memory slot allocated for the given type, then dereferencing it. Like type punning a pointer, but safe even for pointer values, unlike LLVM's bitcast operation.
    (In the past, and possibly at the time of writing, LLVM had a spec bug that implied that memcpying data containing pointers into a buffer where the pointer data will be interpreted as integer values, and then back, is equivalent to conjuring a pointer from the aether and will not be considered with using fully-kosher copies of the original pointer, but this is a bug in LLVM's spec and I expect it to be fixed.)
    (See: https://discourse.llvm.org/t/a-memory-model-for-llvm-ir-supporting-limited-type-punning/61948 )
    (See: https://www.ralfj.de/blog/2022/04/11/provenance-exposed.html )
    (See: http://nhaehnle.blogspot.com/2021/06/can-memcpy-be-implemented-in-llvm-ir.html )
```

Only the above casts are defined and other casts should produce an error. In particular, `unsafe_as` cannot cast to the same type.

### Operators

Operator precedence is defined by konoran's declarative grammar. The grammar is written right-associatively, but has metadata that marks certain nodes as being left-associative; when the parse tree is converted to an AST, these nodes need to be rotated to be left-associative. As a helper, here's a short description of konoran's operator predecence, but remember that the actual definition is in the declarative grammar:

```
not  !    -    +   ~   *   &   @          <- prefix/unary operators
*    /    %    div_unsafe  rem_unsafe     <- infix operators start here
+    -
<<   >>        shl_unsafe  shr_unsafe 
==   !=   >=   <=   >   <
&    |    ^
and  or   &&   ||
```

Operators on a given line have the same precedence and are evaluated left-to-right, i.e. `x + y + z` is evaluated as `(x + y) + z`.

Note that `&&` and `||` have the same precedence. While `&&` can be interpreted as multiplication and `||` can be interpreted as addition, by that line of reasoning, `!=` should be interpreted as addition in that case as well, but giving them all the same precedence would be awful, so konoran abandoned that entire line of reasoning. As primarily a compiler target language, specificational clarity was prioritized over similarity to C; people need to look up C's precedence for these operators all the time, and konoran decided that was unnecessary wasted effort and decided to define them as having the same precedence. As such, `x && y || z` and `x || y & z` are both parsed left to right, as `(x && y) || z` and `(x || y) & z`.

Likewise, note that `>=` etc. have the same precedence as `==` etc.

Postfix operators do not exist.

Parens are used to force evaluation ordering and are not part of the precedence table because they have no evaluation ordering ambiguity.

Ternary conditionals are defined with wrapping parens as part of their syntax and as such are not part of the precedence table.

Assignments cannot occur in an expression context (only a statement context) and as such are not part of the precedence table.

In-place assignments like `+=` do not exist, only simply assignments with `=`.

#### Infix/binary operators

Infix/binary operators are typically only defined for left and right hand sides of the same type, with very few exceptions. The exceptions will be noted.

For numbers:
```
+    addition
-    subtraction
*    multiplication
/    division
%    remainder (not modulo)

==, !=, >=, <=, >, <
    exact equality/inequality
```
The math operators result in the same type. The equality operators result in a u8 containing either 0 (false) or 1 (true).

For integers, operators are defined trivially. (Integers are always two's complement, and integer division truncates the remainder.) For floats, they're defined according to IEEE 754.

Integer division/remainder by zero does not result in undefined behavior. Division by zero results in either zero (for integers) or positive/negative infinity (for floats), and remainder by zero results in zero (for integers) or NaN (for floats). For faster, unsafe integer division/remainder, the following operators are also provided; for them, a right-hand value of zero gives an undefined result:
```
div_unsafe    unsafe division
rem_unsafe    unsafe remainder (not modulo)
```
These operators are not provided for floats because they're not necessary; floating point division/remainder under IEEE 754 is always well-defined.

For integers, the following additional operators are defined:

```
|    bitwise 'or'
&    bitwise 'and'
^    bitwise 'xor'

<<   bitshift left
>>   bitshift right

or, ||    boolean 'or'
and, &&   boolean 'and'
```
The bitwise and bitshift operators result in the same type. The boolean operators result in a u8 containing either 0 (false) or 1 (true).

The right-side value of a bitshift (`<<` or `>>`) must be an unsigned integer of the same size as the type of the left-side integer. In other words, `62i32 >> 2u32` is legal, while `62i32 >> 2i32` and `62i32 >> 2u8` are not.

The left bitshift operator shifts in zeroes. The right bitshift operator shifts in the sign bit if the left-side value is signed, and zeroes otherwise. The other operators are trivially defined.

The bitshift operators are safe and do not result in UB even if the right-hand value is equal to or greater than the number of bits in the input value; in such cases, they result in a maximally-shifted value. For faster, unsafe bitshifting, the following operators are also provided; for them, overflowed bitshifting gives an undefined result:

```
shl_unsafe    unsafe bitshift right
shr_unsafe    unsafe bitshift left
```

Pointers are a special case; they have infix operators, but the right-side must be a pointer-sized unsigned integer. These operators are NOT supported for function pointers. In general, the only things you can do with a function pointer are call it or cast it to a pointer-sized unsigned int. (However, this does mean that performing these operations on an integer derived from a function pointer is instantly UB; rather, it's implementation-defined.)

The following operators are supported for pointers:

```
+    addition
-    subtraction
&    masking
```

The right-side value must be a pointer-sized unsigned integer.

These operations operate on the bitwise representation of the pointer, as though it were an integer. They are equivalent to casting to a u64 and back, with the exception that if the compiler supports provenance/aliasing analysis, these operations MUST retain any relevant provenance/aliasing information. (In other words, adding 50 to a pointer, then subtracting 50 from it, must result in an equivalently-usable pointer value to what you started with, even if the compiler does provenance/aliasing analysis.) The resulting pointer always has a defined value; however, accessing it might cause undefined behavior, depending on how the underlying platform defines such operations.

Having a value magically change between consecutive *volatile* accesses is allowed, i.e. the compiler cannot change the order or number of volatile operations or what addresses they operate on. In particular, volatile writes to even a correctly-derived pointer cannot be optimized away (even if the variable the pointer points at is never used again), and volatile reads from even a correctly-derived pointer must assume that the value may have magically changed since the last time it was accessed (even if it has not been accessed).

#### Prefix/unary operators

The following prefix operator is supported for any variable, and also for aggregate (struct/array) values:

```
&    take address
```

This operation produces a pointer (`ptr(type)`) pointing at the variable's value or the aggregate struct/array's value. If the address of a variable or struct/array value is taken, then that address must be valid for the entire duration of the function. Using such an address after the function has returned is undefined behavior.

"Aggregate (struct/array) values" does not refer to variables containing structs/arrays. Those would be trivial to take the address of even if this language was not used. Rather, this language means that the following code is valid:

```rs
ptr(array(u8, 2)) myptr = &[0u8, 14u8];
```

And is similar in logical behavior to this code:

```rs
array(u8, 2) myvar = [0u8, 14u8];
ptr(array(u8, 2)) myptr = &myvar;
```

So, if the address of a struct/array *value* is taken, it must be given an automatic storage location and pinned there until the function exits (or some other logically equivalent implementation-defined behavior). This is not true of any other value type and other values cannot have their address taken, only variables containing them.

In particular, non-constexpr temporary aggregates pointed to by valid pointers can be modified; building off the above array pointer example, the following code is allowed:

```rs
    (*myptr)[1i64] = 1u8;
```

Pointers derived as above only need to remain valid as long as the code that derives them is actually using them. So, an optimizer can eliminate the storage for a variable or an automatic storage slot if it knows that it's no longer referred to and that all pointers correctly derived from it are dead.

Accessing variables via a pointer to a different type is allowed; for example, accessing a `f32` via a `ptr(u16)` pointing at the first or third byte of the `f32` is allowed. For example, building off the above array pointer example, the following code is allowed:

```rs
u16 myint = *((myptr) bit_as ptr(u16));
```

The target platform may specify access alignment rules for pointers, e.g. accessing a `u16` through a pointer with an odd-numbered integer value might be illegal.

The following prefix operators are supported for floats, and return a new value of the same type:

```
+    do nothing (result in original value)
-    negate (result in negative value)
```

Negating a float is trivially defined for all non-NaN floats as inverting the float's sign. For NaN floats, it is implementation-defined.

The following prefix operators are supported for ints, and return a new value of the same type, except for `!`:

```
+    do nothing (result in original value)
-    negate (result in negative value)
~    bitwise inversion (flip all bits)
!, not    boolean "not" (results in a u8; equivalent to `(val == <zero>)`)
```

In particular, notice that `!`/`not` is not defined for floats. You must use `(float == 0.0<floattype>)` instead.

The above operators are trivially defined.

The following prefix operators are supported for data pointers:

```
*    dereference (evaluate to inner type)
@    make volatile
!    boolean "not" (results in a u8; 1u8 if the pointer is null, 0u8 if the pointer is not null)
```

The `*` operator is not only used when loading a value from a pointer in expressions, but also when assigning to the value pointed to by a pointer, e.g.

```rs
*my_ptr_to_u16 = 162u16;
```

This must also be supported when indexing into arrays, e.g.

```rs
(*my_ptr_to_array_u16)[1i64] = 162u16;
```

For arrays, the following prefix operator is defined:

```rs
decay_to_ptr    convert array of elements to pointer to first element
```

It performs pointer decay, the same type that C performs for arrays at function boundaries. An `array(u8, 4)` will turn into a `ptr(u8)` pointing at the first element of the array. This is logically equivalent to using & on array and casting the resulting pointer to `ptr(inner_type)`.

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
