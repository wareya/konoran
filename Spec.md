# Konoran Spec

## 0 - Preface

Work-in-progress living spec for Konoran, a low-level language designed as a compiler target, with a complexity between C and assembly.

This spec is *very* early, and is subject to change if any horrible design mistakes are discovered.

As an intuition aid, here's an example konoran program:

```php
// a very simple gravity simulation
void main()
{
    f64 yvel = 0.0f64;
    f64 y = 0.0f64;
    f64 gravity = 9.8f64;
    f64 delta = 0.001f64; // timestep
    // need to use half the gravity for timestep-invariant calculation
    gravity = gravity * 0.5f64;
    // konoran's only control flow mechanisms are if, if-else, if-goto, and goto
    u64 i = 0u64;
head:
    if (i < 500000000u64)
    {
        // timestep-invariant single-body gravity calculation
        yvel = yvel + delta*gravity;
        y = y + yvel*delta;
        yvel = yvel + delta*gravity;
        
        i = i + 1u64;
        goto head;
    }
    // an if-goto version would look like:
//  if (i < 500000000u64)
//      goto head;
    // if statements other than just "goto" need curly braces around the body, even if it's a single line, as do else statements
    // print final y coordinate
    print_float(y);
    // type pun first 4 bytes of f64 ad a u32, then print it
    print_float((*((&y) as ptr(u32))) as f64);
    // again, but with a shrunken value
    y = y * (0.1f64);
    print_float((*((&y) as ptr(u32))) as f64);
    // terminating functions must explicitly return even if the return type is void
    return;
}
```

## 1 - Parsing

Konoran program text files are utf-8 **without a BOM**. Line endings are `\n` or `\r\n` and are allowed to be mixed. Konoran's grammar is whitespace-agnostic and doesn't care what indentation you use (or don't).

Konoran's reference implementation uses a PEG-like recursive descent parser generator (a declarative, dynamic one) to parse the program. Implementations must use a parser that accepts and rejects the same programs and produces an equivalent syntax tree (or one that compiles to logically identical code).

Konoran's grammar is specified declaratively:

https://github.com/wareya/konoran/blob/main/src/parser/irgrammar.txt

The declarative grammar is parsed line-by-line; how to parse each line of the declarative grammar is specified as code:

https://github.com/wareya/konoran/blob/main/src/parser/grammar.rs

The reference parser (and lexer/tokenizer) works directly with the declarative grammar (i.e. is a parser generator) and can be read here:

https://github.com/wareya/konoran/blob/main/src/parser/mod.rs

### 1.1 - Tokenization

Token patterns are derived from the grammar (some of which are regexes, and some of which are literals) and matched against the source text in top-to-bottom order (for regexes) or in order of decreasing length (for literals/symbols; with length ties broken by top-to-bottom order) to derive a token stream. Regexes are matched first. Whitespace is used as an explicit token boundary and is stripped. Tokens cannot span lines. C-style, C++-style, and Bash-style (`#`) comments are supported, and are matched for before tokens. Then the token stream is fed to the parser.

### 1.2 - Reserved keywords (lack thereof)

Struct/function/constant/variable names ("struct/symbol names") are allowed to be the same as syntax keywords as long as the resulting declaration/definition is not syntactically ambiguous. The konoran grammar is designed in a way that doesn't need to know any specific struct/symbol names to be able to detect when such a name (rather than a keyword) is being used. If a usage would be visually ambiguous, the grammar determines which interpretation is chosen, regardless of whether or not a conflicting struct/symbol name has been defined.

### 1.3 - Understanding the declarative grammar

The declarative grammar is a list of "grammar points" (e.g. `type`), each of which have a list of "grammar forms" that they can take (e.g. `$ptr_type$`, `$funcptr_type$`, etc), each of which consist of a list of matching rules.

When attempting to match a grammar point to the token stream and find a successful parse, each grammar form is attempted from first to last, and the first successful form is taken (causing the subsequent forms of the point to not be attempted). Each form is a list of matching rules, which can be a literal symbol (e.g. `(`, a grammar point reference (e.g. `$ptr_type$`), a grammar point reference with a modifier (e.g. `$unusedcomma$?` or `$expr$...,`), or a "remaining matches optional" operator (`>>?`).

Each "matching rule" within a grammar point is separated from the next by exactly a single space. Grammar forms are separated from the next grammar point using a single blank line. A grammar form matches the token stream if all of its matching rules finds a match in the token stream, contiguously with no gaps or overlaps, excluding any matching rules after a "remaining matches optional" operator.

#### 1.3.1 - Grammar point modifiers

Grammar points can have metadata terms after them that control how the AST produced after parsing looks:

- `HIDELITERALS` strips root-level literal symbols from this grammar point
- `EMBED` causes the contents of the point to be hoisted in-place into the parent node in the AST (allowed to be arbitrarily many nodes)
- `SIMPLIFY` causes the node to be replaced by its child (must have exactly one child)
- `HIDDEN` causes the node to be entirely thrown away after parsing (not used in konoran's grammar, but supported by its parser generator)
- `TOKEN` causes any regexes found within the grammar form (surrounded by `%`, e.g. `%[0-9]+%`) to be registered as token matchers with the tokenizer; they will be tried 'in order' during tokenization
- `LEFTBINEXPR` causes the grammar form to be interpreted as a right-recursive binary infix expression that needs to be rotated and reprocessed so that it's left-recursive instead; recursive descent parsers can't parse left-recursive grammar rules, and this is a standard workaround

#### 1.3.2 - Grammar form modifiers

In the declarative grammar, grammar point references within a grammar form take the form `$point_name$` with an optional modifier attached at the end. The supported modifiers are:

- `?` e.g. `$expr$?` makes matching the grammar point optional; if failed, the current token location will be tried with the next matching rule in the grammar form, without generating a failure
- `*` e.g. `$expr$*` allows the grammar point to occur zero or more times
- `+` e.g. `$expr$+` allows the grammar point to occur one or more times
- `...X` e.g. `$expr$...X` allows the grammar point to occur one or more times, but each repetition must be separated by the single character X (e.g. `$expr$...,` matches a list of `expr` matches separated by commas, like `1.0f64, 0u8`)
- `+..($another_point$)` e.g. `$rhunexpr_right$+..($funcargs$)` allows the grammar point to occur one or more times, but the final instance of it must be a single particular grammar point inside, or else the matching rule fails to match.

#### 1.3.3 - 'remaining matches optional' operator

Assuming the matching rules to the left of the `>>?` operator match, the remaining matching rules in the given grammar form are optional and are only included if they successfully match. If they do not successfully match they are all discarded and the grammar form is considered to have parsed successfully as only the prior matching rules.

## 2 - Programs

Konoran programs consist of a number of modules linked together, possibly dynamically, and possibly with non-Konoran modules. The specific semantics of linking are entirely implementation-defined.

Standalone konoran programs usually export a `main` function that can be executed by an OS.

### 2.1 - Static initialization

Konoran programs have statically-initalized data. Sometimes this data can only be initialized by running code. In such cases, that initialization code must be run before any function in the Konoran program is called, and it must be run exactly once. This initialization code might call out to non-Konoran code, and the environment must tolerate this. Static initialization code is run from top to bottom.

### 2.2 - Static initialization linking order

The above-described initialization code comes from each individual module. When linking multiple modules together into a single program, the exact order in which each module's initialization code is run is implementation-defined; however, implementations are strongly encouraged to allow the user to control it. For example, if you link a user's modules called `main` and `mathlib` together with the command `link main.o mathlib.o`, the implementation could specify that the initializers from `main.o` should run before those from `mathlib.o` because it was listed first in the linking command; such behavior would comply with this encouragement.

## 3 - Modules

Konoran modules do not have a preprocessor, do not "include" or "use" specific other modules, are not logically namespaced, and can be compiled independently of one another in any order before linking.

### 3.1 - Module contents

Konoran modules consist of a series of:

- Struct type definitions
- Global variable/constant definitions
- Global variable imports
- Function definitions
- Function imports

These can come in any order, including interleaved order, and there are no pre-declarations for things that are defined in the current module. The implementation must run a pre-pass over the module to collect definitions and imports.

### 3.2 - Module global variable and constant semantics

Global variable definitions can contain arbitrary expressions and are evaluated from top to bottom on program initialization. It's strongly encouraged that advanced implementations fold logically constant initializers for global variables down into static data even if the variables are not marked as constant, but this is not mandatory. Global variables do not require initializers, but note that without them, their initial state is undefined.

Global variables are mutable and can be modified and rewritten.

#### 3.2.1 - Global constants

Global variables marked with the `constexpr` keyword are not variables; they are global constants.

Global constants must have initializers, and those initializers must be folded down to specific values at compile time. Global constants must be stored in static data if the compiler supports static data. Otherwise, they may be loaded into memory during initialization, but must be loaded before any other initializers are run.

Global variables and constants are visible to the bodies of all functions defined in the current module, including functions defined above them.

Global constant initializers *must* be folded down into known values at compile time, and *require* initializers. Global constant initializers cannot contain function calls as konoran doesn't support constexpr functions, only constexpr expressions.

Global constants may not be modified and the compiler is allowed to assume that they never change from what is computed during at compile time.

Global constants are not visible to other modules.

### 3.3 - Module component redeclaration rules

Within a single module, two structs cannot have the same name, two functions cannot have the same name, and two global constants or global variables cannot have the same name.

### 3.4 - Module component visibility rules

The initializers of global variables/constants can see and refer to the values of earlier global variables/constants that were defined above them in the module file, and not ones that are defined below them.

Structs are not exposed to other modules. If two modules want to use the same struct they need to redefine it.

Functions and global variables (not global constants) can have the following visibility specifiers:

- `export_extern` - export to anywhere possible, possibly even as a DLL export, but also to other modules
- `<no modifier>` - export to other modules
- `private` - only expose to current module, do not allow other modules to find
- `using` - find symbol in other modules; local definition is illegal
- `import_extern` - find symbol from anywhere possible, possibly even from DLLs, but also from other modules; local definition is illegal

The exact semantics of `export_extern` and `import_extern` are implementation-defined, except that they must be the same as or supersets of the default or `using` visibility respectively.

Global constants are always logically private.

The exact semantics of linking are implementation-defined.

### 3.5 - Module C ABI compliance when linking

Functions with non-private visibility are imported and exported according to the platform's C ABI, unless struct/array values (not struct/array pointers) pass in or out of the function.

#### 3.5.1 - Aggregate values on the ABI boundary

Functions that have array or struct values pass through them are not guaranteed for conform to the platform's C ABI, and may have different implementation-defined rules for what parts of the aggregate are passed in which registers or where on the stack or in memory in what situations.

Pointers to arrays/structs are fine and must be passed according to the platform's C ABI.

## 4 - Functions

Functions have return types, names, argument lists, and bodies. A function's signature consists of its return type and the list of its argument types. Function bodies consist of a series of local variable declarations/definitions, statements, labels, and branching statements (if statements and gotos), in any order.

Functions are defined like follows:

```php
<returntype> name ( type1 arg1, type2 arg2, ... )
{
    // list of statements that must always either return or enter into an infinite loop
}
```

Functions are imported from other modules like follows:

```php
[using|import_extern] <returntype> func_name ( type1 arg1, type2 arg2, ... );
```

The exact syntax is specified in the grammar.

The `void` type only exists for the sake of functions that return no value; variables and pointers cannot have the type `void`.

### 4.1 - Function control flow

Branches can point "upwards", not just downwards. Branches can only point to labels within the current function; they cannot point to the insides of other functions. Function bodies must return or enter an infinite loop in all control paths, but are allowed to have redundant returns that create dead code. In other words, falling through to the end of the function without a return is illegal code and must produce an error. The return value must be of the same type as is declared in the function's signature. `void` functions return no value, but must still explicitly return.

### 4.2 - Function variable mutability

Variables are mutable and can be rewritten. Compilers are free to detect and mark variables as const and optimize them away or fold/inline them, even if their address is taken, as long as the underlying value is never modified via correctly-derived pointer.

Reading an uninitialized variable (local or global) results in an undefined or poison value that can take on any value (even if the variable is only ever accessed once) and do anything, classic undefined behavior. If such a value is given to the `freeze` operator, it will be frozen into a then-known state; however, this state might not reflect what was actually present in the memory of the uninitialized variable.

Variables marked with the `constexpr` keyword are not variables; they are local constants. They must be defined upon declaration and cannot be modified/rewritten. Writing to a local constant via a pointer is undefined behavior.

#### 4.2.1 - Local constants

Local constants follow the same rules as global constants (e.g. their initializer must be known at compile time, they cannot be modified, etc), except they are only visible to the function that defines them.

Local constants follow block scope rules just like normal local variables. Visible local constants have a defined value determined by their initializer even if the flow of execution never passed through the statement defining the local constant.

Local constants and local variables are only visible after their declarations.

### 4.3 - Function stack consistency

Jumps, including goto, are incapable of leaving the stack in an invalid state. Variable declarations in functions (and their arguments) only last for the body of the function and do not affect other functions. Function-specific variable declarations are allowed to "shadow" global variables and functions; doing so must not produce an error or warning.

#### 4.3.1 - Function variable behavior with regards to scopes

When a function begins, its arguments are treated as normal variables, with their values pre-filled, in a "zeroth" scope. The function body begins the "first" scope, and can declare local variables that shadow its arguments. Other block scopes (defined in the grammar as `$statementlist$`) open new scopes, which can shadow variables created in outer scopes. Importantly, scopes do not logically grow or shrink the stack, so `goto`ing into them past variable declarations is not instant UB; shadowing and scoping are purely a name lookup concern, and the function must logically behave as though all variable declarations were invisibly hoisted to the top of the function (except for name lookup and shadowing). The implementation is highly encouraged to perform disjoint stack slot reuse optimizations to reduce how much stack space a given function uses up with variables declared in disjoint block scopes.

#### 4.3.2 - Function stack variable behavior

If a variable is declared in an inner scope, and its pointer is not leaked into an outer scope, it becomes inaccessible once that scope ends, and the optimizer is allowed to make this assumption. However, variables must 'exist' as long as a correctly-derived pointer to them is held within the same function. If their address was taken, their stack slot cannot be eliminated until no correctly-derived pointer is capable of referring to it, or the function returns or enters an infinite loop that cannot see it.

The following code is legal; read the comments for more rationale:

```php
    ptr(f64) magic_ptr;
    {
        constexpr f64 gkue = 19843.81f64;
        f64 gkue2 = 5481.581f64;
        magic_ptr = &gkue2;
    }
    //print_float(gkue); // invalid; no longer logically exists
    //print_float(gkue2); // invalid; no longer logically exists
    
    print_float(*magic_ptr);
    // the above memory access is VALID! declarations and block scopes are NOT runtime stack manipulation commands!
    // local variables must remain accessible until whichever of the following happens sooner:
    // 1. no correctly-derived pointers to them can exist
    // 2. the function exits
    // note that point 1 means that variables may still be valid even if no pointers point directly at the variable's address range
    // as long as such a pointer may be correctly derived (e.g. an OOB pointer is turned back into an in-bounds pointer)
    //
    // This may seem strange, but it's reasonable with how "real" optimizing compilers process data flow.
    // "Real" optimizing compilers can handle this no problem.
    // And non-optimizing compilers can just allocate all possible variables on the stack when the function first enters.
    //
    // If you compile konoran code down to LLVM IR that `alloca`s every possible variable in the header of the function,
    //  it'll optimize unnecessary `alloca`s away and also keep the stack as small as possible, automatically,
    //  without breaking inner-scope variables whose addresses leaked to outer scopes. That's an example of a "real"
    //  optimizing compiler handling this with no problem.
```

`goto`ing over a declaration is not UB, as any code that can see the variable's name is allowed to legally access it; variable declarations are not runtime stack manipulation instructions.

Attempting to derive a pointer to one local variable from a pointer to another local variable is UB and the optimizer is allowed to assume that a konoran program does not do this.

#### 4.3.2 - Function stack variable pointer leakage

Attempting to conjure a pointer to a local variable that is not currently in-scope is UB because conjuring pointers to variables is UB, and the optimizer is allowed to assume that a konoran program does not do this.

### 4.4 - Functions do not have static variables

Unlike C, konoran does not have static function variables. Global variables must be used instead.

### 4.5 - Function calls

Functions are called with a syntax similar to C, e.g.

```php
print_float(8243.9f64);
```

Function arguments are fully evaluated one at a time, from left to right.

Function calls evaluate to a value of whatever type the function returns. If the function returns `void`, it evaluates to no value and cannot be used in expressions (because there's nothing an expression or statement can do with another expression of type `void`, inevitably leading to an error).

### 4.6 - Statement evaluation order

Statements within a function are evaluated top to bottom as control flow passes through the function. When a `goto` is encountered, it is followed. When an `if` statement is encountered, a particular branch from that if statement is followed. Which branch is taken after an if statement is trivially defined.

Assignments are evaluated left-side-first. So, if you have a function that returns a pointer, and assign into it, the function is evaluated before the expression you assign into it:

```php
u8 sdklf = 15u8;
ptr(u8) myfunc_1()
{
    print_float(01011001.0f64);
    return &sdklf;
}
u8 myfunc_2()
{
    print_float(20201.20201f64);
    return 81u8;
}
void main()
{
    // myfunc_1 runs before myfunc_2
    *myfunc_1() = myfunc_2();
}
```

#### 4.6.1 - Declaration initializer evaluation order

Declarations with initializers evaluate the initializer before creating the variable being declared. For example:

```php
f64 shadowed = 8143.81f64;
{
    // `shadowed` in the below expression evaluates to the earlier-declared `shadowed` variable,
    //   not the new not-yet-initialized `shadowed` variable.
    f64 shadowed = shadowed + 1000.0f64;
    // must print 9143.81 (sans rounding)
    print_float(shadowed);
}
```

### 4.7 - Control flow syntax

The supported control flow constructs are very limited. See [irexample6.knr] for examples of all possible control flow uses (goto, if-goto, if, if-else, and if-else with an if-else inside the else, as well as goto jumping over a variable definition). In short:

```php
// goto
goto label;
// if-goto
if(cond) goto label;
// if (braces mandatory)
if(cond) { statement(); }
// if-else (braces mandatory)
if(cond) { statement(); } else { statement(); }
// if-else with an if inside of the else
if(cond) { statement(); } else if(cond2) { statement(); }
// if-else with an if-else inside of the else
if(cond) { statement(); } else if(cond2) { statement(); } else { statement(); }
// the above two examples are equivalent to the following, which is also valid:
if(cond) { statement(); } else { if(cond2) { statement(); } }
if(cond) { statement(); } else { if(cond2) { statement(); } else { statement(); } }
```

The braces are mandatory even if there is only one statement. There can be multiple statements inside the braces, and they're executed from top to bottom.

There is no braceless goto variant for compound if-else statements. "if-goto" is considered its own type of statement with its own syntax, not a specialization of the "if" statement syntax.

## 5 - Types

Konoran is strongly typed and has primitive/intrinsic numeric types, struct types, array types, and pointer types (including function pointers).

Konoran has a `void` type, but it is only used as the return type of functions that do not return a value. It is forbidden in all other contexts, including as a pointee type for pointers. (Type punning via pointers is explicitly allowed; instead of void pointers, use `ptr(u8)`.)

### 5.1 - Numeric types

Konoran has the following primitive/intrinsic numeric types:

```php
u8, i8, u16, i16, u32, i32, u64, i64, f32, f64
```

These are unsigned and signed integers (two's complement) and IEEE-compliant binary floating point numbers. u8 has a bit width of 8 and a byte width of 1, u16 has a bit width of 16 and a byte width of 2, etc.

The implementation is allowed to assume that signaling NaNs act like quiet NaNs in arbitrary situations, i.e. operating on a NaN is not guaranteed to have signalling side effects.

### 5.2 - Pointer types

Konoran has the following pointer types:

```php
ptr(type)
funcptr(returntype, (arg1type, arg2type, ...))
```

Pointers can be casted back and forth with `u64` (or whatever the target's pointer-sized int type is) using the `bit_as` operator (e.g. `(my_ptr) bit_as u64`). With regards to the values stored in this u64 after casting, the in-memory "size" of a u8 must be 1, of a u16 must be 2, etc.

### 5.3 - Composite types

Konoran has the following composite types:

```php
struct <name> { type1 var1, type2 var2, ... }
array(type, len)
```

Zero-size composite types are illegal.

#### 5.3.1 - Struct type semantics

Struct types consist of a series of members (also called variables, properties, etc) declared one at a time, with no specified value. Struct values consist of a series of values with the appropriate type for the struct being constructed, and are prefixed with the name of the struct (e.g. `Vec2 { 1.0f32, 0.5f32 }`).

Structs have a fixed, known size, and their members are not reordered. Structs do not have any invisible padding; even if `u32`s need to be aligned to 4 bytes on the target platform, a struct that goes `struct ... { u8 a; u32 b; }` will have the `b` property start at the second byte of the struct, and the struct will be 5 bytes long. Alignment must be done manually. To make this easier, structs can have multiple members with the name `_`, none of which are accessible, and are assumed to have an unknown (possibly undefined/poison) value if accessed with pointer arithmetic. It should be noted that padding around floats should ideally be of a float type, and same for non-floats. For example:

```php
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

#### 5.3.2 - Composite type passing/copying

Composite types (structs/arrays) are passed by value when calling functions with them as arguments, or returning them from functions.

#### 5.3.3 - Array semantics

Array types have a size of the size of their inner type multiplied by their length. Structs have a size of the sum of the sizes of their inner types.

#### 5.4 - Type punning

Type punning is allowed and legal.

#### 5.4.1 - Pointer casting

Casting pointers to `u64` (or, on 32-bit architectures, `u32`) and back is allowed, and is allowed to either retain or destroy provenance information, but must be considered a correctly-derived pointer to the pointed-at object if the optimizer does provenance/alias analysis.

### 5.5 - Misaligned structs

The konoran implementation is allowed, but not encouraged, to produce an error if structs have misaligned members for the target platform.

### 5.6 - Numeric literals

Numeric literals are tokenized and interpreted using regexes, and must be terminated by a suffix that indicates their type.

#### 5.6.1 - Integer literals

For integers, the following regexes are used:

```regex
[\-]?[0-9]+(i|u)(8|16|32|64)
[\-]?0x[0-9A-Fa-f]+(i|u)(8|16|32|64)
```

If the non-`0x` regex finds a match, the number is interpreted as an integer of the given type written in decimal. If the given value exceeds the capacity of the type, the compiler must throw an error. Importantly, `-128i8` must immediately be interpreted as a maximally negative `i8`, not as an `i8` with an overflown value that is then negated with a unary operator; the `-` is part of the regex for a reason.

#### 5.6.2 - Floating-point literals

For floats, the following regexes are used:

```regex
[0-9]+\.[0-9]+([eE]([+-])?[0-9]+)?f(32|64)
[0-9]+\.([eE]([+-])?[0-9]+)?f(32|64)
\.[0-9]+([eE]([+-])?[0-9]+)?f(32|64)
```

After removing the type suffix, the float literal must be parsed the same way that a correct C compiler would parse it, with the same rules for what `e` and `E` mean, etc.

Values that overflow past what can be stored in the given float type result in (positive/negative) infinity, not an undefined or unspecified value.

### 5.7 - Array literals

Array literals are a list of values separate by a comma with an optional comma at the end, sandwiched between `[` and `]`. They immediately evaluate to the correct type of array with exactly the right length. The values do not need to be literals. The values must all be of the same type. The values are fully evaluated one at a time, from left to right.

```php
array(u8, 2) myarray = [0u8, 14u8];
//array(u8, 5) myarray2 = [0u8, 14u8]; // invalid; wrong length
```

### 5.8 - Struct literals

Struct literals have a prefix of the name of their struct type, followed by a list of member values sandwiched between `{` and `}`. The values do not need to be literals. The values must have the correct types. The values are fully evaluated one at a time, from left to right.

```php
struct Vec2
{
    f32 x;
    f32 y;
}
// ...
Vec2 my_vec2 = Vec2 { 124.016f32, 815.1538f32 };
//Vec2 my_vec2_2 = Vec2 { 115u32, 815u32 }; // invalid; values have wrong type
```

### 5.9 - Char literals

Konoran has char (pronounced 'care', as in 'character') literals that compile down to a u8 or u32 literal. u8 literals are valid for any unicode character with codepoint numbered 0xFF (i.e. U+00FF, 'ÿ') or less. u32 literals are valid for any unicode codepoint. This must be done at compile time and be exactly equivalent to using a normal integer literal. Char literals support escape sequences, specifically `\n`, `\r`, `\t`, `\'`, and `\\`. Char literals do not support numeric escape sequences.

```php
'0' // 0x30u8
'ÿ' // 0xFFu8
//'𠂌' // not supported
'0'u32 // 0x00000030u32
'あ'u32 // 0x00003042u32
'𠂌'u32 // 0x0002008Cu32
'\r' // 0x0Du8
'\n' // 0x0Au8
'\t' // 0x09u8
'\'' // 0x27u8
'\\' // 0x5Cu8
//'\0' // not supported, use 0u8
```

### 5.10 - String literals

Konoran has utf-8 string literals. They compile down either to an array literal or to a pointer to const data, depending on which syntax is used. String literals support basic escape sequences, specifically `\n`, `\r`, `\t`, `\"`, and `\\`. String literals do not support numeric escape sequences. The string data as accessed by the konoran program is utf-8.

```php
array(u8, 7) my_array = constexpr "skgue\n"array;
ptr(array(u8, 7)) my_array_ptr = &"skgue\n"array;
ptr(u8) my_ptr = "skgue\n";
//ptr(ptr(u8)) my_addr_of_ptr = &"skgue\n"; // invalid; cannot get the address of an evaluated pointer value
```

It's also possible to use string literals that lack a null terminator, as follows:

```php
array(u8, 6) my_array = constexpr "skgue\n"array_nonull;
ptr(array(u8, 6)) my_array_ptr = &"skgue\n"array_nonull;
ptr(u8) my_ptr = "skgue\n"nonull; // dangerous!!!
```

## 6 - Volatile memory access

There is no `volatile` variable modifier. However, pointer values can be marked as having volatile access semantics using the `@` operator. So, to use an mmio register, you might use something like the following code:

```php
ptr(u32) my_mmio_reg = (0x80000030u64) as ptr(u32);
u32 my_val = *@my_mmio_reg; // @ causes * to perform a volatile load
*@my_mmio_reg = my_val + 10u32; // @ causes = to perform a volatile store
```
Given the above context, the following would NOT perform a volatile operation:
```php
@my_mmio_reg = (0x80000040u64) as ptr(u32); // the volatility modifier does nothing in this case; the pointer value is just replaced
```

This more closely reflects how compiler backends tend to work than a `volatile` variable modifier. As such, konoran chose this approach to exposing volatile memory operations, rather than a variable modifier.

### 6.1 - Volatile memory access rules

Volatile memory acceses must be treated as though they have (non-UB-producing) side effects. In particular, optimizations must not change volatile memory accesses in order, number, or address. Implementations are allowed to specify arbitrary side-effects for volatile memory accesses. Volatile memory accesses are not typically assumed to modify variables or memory that they do not point at, but implementations are allowed to specify situations where they do. For example, an implementation might specify that doing a volatile read of a status register can cause an interrupt handler to run; normally, the implementation's optimizer is allowed to assume that this interrupt handler doesn't modify any variables or memory that the current non-interrupt code is working on, but the implementation is allowed to specify that it does, and prevent its own optimizer from assuming that those variables or memory are unchanged during the interrupt. If the implementation does not specify such a situation, then it is assumed that volatile memory accesses do not alter unrelated variables/memory, and the optimizer is free to use that assumption.

Having a value magically change between consecutive *volatile* accesses is allowed, i.e. the compiler cannot change the order or number of volatile operations or what addresses they operate on. In particular, volatile writes to even a correctly-derived pointer cannot be optimized away (even if the variable the pointer points at is never used again), and volatile reads from even a correctly-derived pointer must assume that the value may have magically changed since the last time it was accessed (even if it has not been accessed).

## 7 - Casts

Konoran has three casting operators: `as`, `unsafe_as`, and `bit_as`. They are defined over the following type pairs and have the given behaviors. Note that it is not possible to change both the signedness and size of an integer in a single cast operation.

### 7.1 - Value casts

```
(val) as <val type>
    Does nothing. Returns original value.

(pointer val) as <different pointer type>
    Reinterprets the pointer as pointing at a different type. Legal even if casting between function pointer and normal pointer; however, the resulting pointer may be illegal to use depending on how the platform defines the behavior of such an operation. (The existence of illegal-to-use pointers in memory is not, itself, UB or illegal.)

(integer_val) as <integer type of same size>
    Converts an integer to an integer of a different signedness. Does not modify the underlying bit representation of the integer.

(integer_val) as <integer type of same signedness>
    Truncates or extends an integer to a different size. When truncating, upper bits are dropped. When extending, signed integers get sign-extended, and unsigned integers get zero-extended.

(float_val) as <different float type>
    Converts a floating-point number from one width to another. If the number doesn't have a nearby value in the target type, either positive/negative infinity or a minimal/maximal value is given (usually positive/negative infinity).

(int_val) as <float type>
    Converts an integer to a floating-point number. If the integer cannot be represented exactly, the closest value is given.

(float_val) as <integer type>
    Converts an floating-point number to an integer type. If the value is outside of the integer range, either the minimum or maximum integer value is given, whichever is closer. If the value is NaN, either zero, maximum, or minimum integer value is given, according to the implementation.
```
### 7.2 - Unsafe value casts
```
(float_val) unsafe_as <integer type>
    Converts an floating-point number to an integer type. If the value is outside of the integer range, the result is undefined.
```
### 7.3 - Bitcasts
```
(val) bit_as <val type>
    Does nothing. Returns original value.

(pointer val) bit_as <different pointer type>
    Reinterprets the pointer as pointing at a different type. Same semantics as a normal pointer-to-pointer cast.

(pointer_val) bit_as <u64> (or u32 on 32-bit architectures)
    Converts a pointer to an integer-based representation.

(ptr_sized_int_val) bit_as <pointer type>
    Converts an integer to a pointer. When a pointer is casted to u64 (or u32 on 32-bit architectures) and then back to the same pointer type, it must point to the same object and memory location.
    If a pointer to a stack value is cast to int and back, it cannot be assumed to point inside the address range of the original stack value, so accessing it is not immediately UB.
        (see [examples/pointer_conjuration_test.knr], `main()`)
    If the compiler has provenance or aliasing analysis, the resulting pointer value is treated as being derived any value that contributed to the calculation of the given integer.
        (see [examples/pointer_conjuration_test.knr], `main2()`)

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
### 7.4 - Unsupported casts

Only the above casts are defined and other casts should produce an error. In particular, `unsafe_as` cannot cast to the same type.

## 8 - Operators

Konoran has both binary/infix and unary/prefix operators. Some expressions are not defined as operators. The grammar defines which expressions use operators and which do not.

### 8.1 - Operator precedence

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

Operators on a given line have the same precedence and are evaluated left to right, i.e. `x + y + z` is evaluated as `(x + y) + z`.

#### 8.1.1 - Precedence design note

Note that `&&` and `||` have the same precedence. While `&&` can be interpreted as multiplication and `||` can be interpreted as addition, by that line of reasoning, `!=` should be interpreted as addition in that case as well, but giving them all the same precedence would be awful, so konoran abandoned that entire line of reasoning. As primarily a compiler target language, specificational clarity was prioritized over similarity to C; people need to look up C's precedence for these operators all the time, and konoran decided that was unnecessary wasted effort and decided to define them as having the same precedence. As such, `x && y || z` and `x || y & z` are both parsed left to right, as `(x && y) || z` and `(x || y) & z`.

Likewise, note that `>=` etc. have the same precedence as `==` etc.

### 8.2 - Operator design choices and non-issues

Postfix operators do not exist.

Parens are used to force evaluation ordering and are not part of the precedence table because they have no evaluation ordering ambiguity.

Ternary conditionals are defined with wrapping parens as part of their syntax and as such are not part of the precedence table.

Assignments cannot occur in an expression context (only a statement context) and as such are not part of the precedence table.

In-place assignments like `+=` do not exist, only simple assignments with `=`.

### 8.3 - Infix/binary operators

Infix/binary operators are typically only defined for left and right hand sides of the same type, with very few exceptions. The exceptions will be noted.

When evaluating an infix operator, the operands are evaluated left-first, right-second.

#### 8.3.1 - General numeric infix operators

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

For integers, operators are defined trivially. Integers are always two's complement, with overflow being wraparound (even for signed integers), and integer division truncates the remainder (towards zero). For floats, they're defined according to IEEE 754.

#### 8.3.1.1 - Integer-specific unsafe numeric infix operator variants

Integer division/remainder by zero does not result in undefined behavior. Division by zero results in either zero (for integers) or positive/negative infinity or NaN (for floats, depending on whether or not the numerator is zero), and remainder by zero results in zero (for integers) or NaN (for floats). For faster, unsafe integer division/remainder, the following operators are also provided; for them, a right-hand value of zero gives an undefined result:

```
div_unsafe    unsafe division
rem_unsafe    unsafe remainder (not modulo)
```

These operators are not provided for floats because they're not necessary; floating point division/remainder under IEEE 754 is always well-defined.

#### 8.3.2 - Integer-specific infix operators

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

The `and`/`or`/`&&`/`||` operators do NOT short-circuit.

The left bitshift operator shifts in zeroes. The right bitshift operator shifts in the sign bit if the left-side value is signed, and zeroes otherwise. The other operators are trivially defined.

##### 8.3.2.1 - Unsafe bitshift operator variants

The bitshift operators are safe and do not result in UB even if the right-hand value is equal to or greater than the number of bits in the input value; in such cases, they result in a maximally-shifted value. For faster, unsafe bitshifting, the following operators are also provided; for them, overflowed bitshifting gives an undefined result:

```
shl_unsafe    unsafe bitshift right
shr_unsafe    unsafe bitshift left
```

#### 8.3.3 - Pointer infix operators

Pointers are a special case; they have infix operators, but the right-side must be a pointer-sized unsigned integer. These operators are NOT supported for function pointers. In general, the only things you can do with a function pointer are call it or cast it to a pointer-sized unsigned int. (However, this does mean that performing these operations on an integer derived from a function pointer is instantly UB; rather, it's implementation-defined.)

The following operators are supported for pointers:

```
+    addition
-    subtraction
&    masking
```

The right-side value must be a pointer-sized unsigned integer.

The resulting pointer from these operators is not considered "conjured" and, if the compiler has provenance analysis, has provenance from the original pointer (or, at least, accesses to the value under the original pointer must consider the resulting pointer as aliasing it until the resulting pointer and anything derived from it stops existing).

##### 8.3.3.1 - Pointer infix operator semantics

These operations operate on the bitwise representation of the pointer, as though it were an integer. They are equivalent to casting to a u64 and back, with the exception that if the compiler supports provenance/aliasing analysis, these operations MUST retain any relevant provenance/aliasing information. (In other words, adding 50 to a pointer, then subtracting 50 from it, must result in an equivalently-usable pointer value to what you started with, even if the compiler does provenance/aliasing analysis.) The resulting pointer always has a defined value; however, accessing it might cause undefined behavior, depending on how the underlying platform defines such operations.

In particular, the `+` and `-` operators do not work like they do in C; they operate on byte-sized pointer logic, not word-sized pointer logic, so, for example, adding `1` to an already-aligned `ptr(u64)` will result in an unaligned pointer pointing one byte after the start of the `u64`.

#### 8.4 - Prefix/unary operators

Konoran has prefix/unary operators for certain types, and for any type in certain situations.

##### 8.4.1 - Pointer-to / Address-of operator

The following prefix operator is supported for any variable, and also for aggregate (struct/array) values:

```
&    take address
```

This operation produces a pointer (`ptr(type)`) pointing at the variable's value or the aggregate struct/array's value. If the address of a variable or struct/array value is taken, then that address must be valid for the entire duration of the function. Using such an address after the function has returned is undefined behavior.

##### 8.4.1.1 - Pointer-to clarification for aggregate values

The phrasing "aggregate (struct/array) values" does not refer to variables containing structs/arrays. Those would be trivial to take the address of even if this phrasing was not used. Rather, this language means that the following code is valid:

```php
ptr(array(u8, 2)) myptr = &[0u8, 14u8];
```

And is similar in logical behavior to this code:

```php
array(u8, 2) myvar = [0u8, 14u8];
ptr(array(u8, 2)) myptr = &myvar;
```

So, if the address of a struct/array *value* is taken, it must be given an automatic storage location and pinned there until the function exits (or some other logically equivalent implementation-defined behavior). This is not true of any other value type and other values cannot have their address taken, only variables containing them.

In particular, non-constexpr temporary aggregates pointed to by valid pointers can be modified; building off the above array pointer example, the following code is allowed:

```php
    (*myptr)[1i64] = 1u8;
```

Pointers derived as above only need to remain valid as long as the code that derives them is actually using them. So, an optimizer can eliminate the storage for a variable or an automatic storage slot if it knows that it's no longer referred to and that all pointers correctly derived from it are dead.

If the address-of operator is used on a struct/array value in a loop, then the same storage location is used for that given address-of operator on each successive iteration of the loop, however that storage location must still remain valid until the function exits (or until the compiler knows that it cannot be legally accessed). For example:

``php
loophead:
    ptr(array(u8, 2)) myptr = &[0u8, 14u8];
    ptr(array(u8, 2)) myptr2 = &[0u8, 14u8];
    if (read_input() == '\n') // assume that such a function exists
        return;
    goto loophead;
```

In the above example, `myptr` has the same value on each iteration, and `myptr2` has the same value on each iteration. However, they have different values, i.e. point at arrays in different storage locations. Each time the loop runs, the arrays inside the storage locations are refreshed with the array values whose addresses are being taken, before their addresses are taken.

##### 8.4.1.2 - Type punning with correctly-derived pointers

Accessing variables via a pointer to a different type is allowed; for example, accessing a `f32` via a `ptr(u16)` pointing at the first or third byte of the `f32` is allowed. For example, building off the above array pointer example, the following code is allowed:

```php
u16 myint = *((myptr) bit_as ptr(u16));
```

The target platform may specify access alignment rules for pointers, e.g. accessing a `u16` through a pointer with an odd-numbered integer value might be illegal.

#### 8.4.2 - Floating-point prefix operators

The following prefix operators are supported for floats, and return a new value of the same type:

```
+    do nothing (result in original value)
-    negate (result in negative value)
```

Negating a float is trivially defined for all non-NaN floats as inverting the float's sign. For NaN floats, it is implementation-defined.

#### 8.4.3 - Integer prefix operators

The following prefix operators are supported for ints, and return a new value of the same type, except for `!`:

```
+    do nothing (result in original value)
-    negate (result in negative value)
~    bitwise inversion (flip all bits)
!, not    boolean "not" (results in a u8; equivalent to `(val == <zero>)`)
```

In particular, notice that `!`/`not` is not defined for floats. You must use `(float == 0.0<floattype>)` instead.

`-` does not result in UB when used with maximally negative signed integers; it results in the same value being given back. In other words, `-(-128i8)` evaluates to `-128i8`.

The other above operators are trivially defined.

##### 8.4.3 - Pointer prefix operators

The following prefix operators are supported for data pointers:

```
*    dereference (evaluate to inner type)
@    make volatile
!    boolean "not" (results in a u8; 1u8 if the pointer is null, 0u8 if the pointer is not null)
```

##### 8.4.3.1 - Dereference operator semantics

The `*` operator is not only used when loading a value from a pointer in expressions, but also when assigning to the value pointed to by a pointer, e.g.

```php
*my_ptr_to_u16 = 162u16;
```

This must also be supported when indexing into arrays, e.g.

```php
(*my_ptr_to_array_u16)[1i64] = 162u16;
```

#### 8.4.4 - Array prefix operators

For arrays, the following prefix operator is defined:

```php
decay_to_ptr    convert array of elements to pointer to first element
```

It performs pointer decay, the same type that C performs for arrays at function boundaries. An `array(u8, 4)` will turn into a `ptr(u8)` pointing at the first element of the array. This is logically equivalent to using `&` on array and casting the resulting pointer to `ptr(inner_type)`.

### 8.5 - Constant expressions

Konoran has a `constexpr` pseudo-operator that looks like e.g. `constexpr (16u64)` or `constexpr (85u8 + 14u8)`.

Expressions inside of the `constexpr` operator must be guaranteed to be const-foldable down to a specific literal value at compile time, with no runtime computation of the inside result. However, if the implementation has separate "optimizations" and "no optimizations" modes, the "no optimizations" mode is allowed to not perform constant folding. If there are no such separate modes, the implementation must perform constant folding.

If a `constexpr` expression is being assigned to a `constexpr` variable, then it must be folded down during compilation, even if the implementation has a "no optimizations" mode and is configured to use it. Then, if the implementation supports static memory, this variable must be stored in static memory, not loaded into memory piecemeal at runtime.

## 9 - Generally undefined behaviors

The following program behaviors are generally undefined and the compiler is allowed to assume that they never happen for the sake of optimization:

1) Accessing a variable without using its name or a correctly-derived pointer to it
2) Calling a function without using its name or a correctly-derived pointer to it

The above two rules do not apply to `export_extern`'d global variables or functions, because `export_extern` implies that such global variables or functions can have pointers to them derived "correctly" by external code (e.g. by a symbol table lookup).

In some cases, the implementation is allowed to define new UB of its own.

### 9.1 - Conjured variable access

Point 1 means that other code using that variable is allowed to assume that it doesn't suddenly change for no reason, even if an incorrectly-derived pointer value might be pointing to it. For example:

```php
u32 x = 0;
ptr(u32) maybe_x = (randi()) as ptr(u32);
*maybe_x = 16u32;
print_float((x) as f64);
```

The above program contains undefined behavior if `randi()` is capable of producing a value matching the address of the `x` variable. As such, a compiler optimizing this code is allowed to assume that the value passed to `print_float()` remains `0.0f64`, even if `*maybe_x` is written to in the meantime, because even if `maybe_x` points to `x`, it was not correctly derived.

Importantly, point 1 does not make it instant UB to access/modify data via conjured pointers. The optimizer is merely allowed to assume that accesses via conjured pointers do not modify named variables. Arbitrary memory and heap memory are assumed to be disjoint from "variables".

### 9.2 - Executing 'dead code' functions

Point 2 means that the optimizer can remove functions that are never referenced even if code might accidentally construct the value of a function pointer that would point at that function if it hadn't been removed. The same is true of point 1 and variables. However, this does not apply to `export_extern`'d global variables or functions, nor does it apply to default-visibility global variables or functions *before linking*. (After linking, it once again applies to global variables and functions, as long as they have default visibility and not `export_extern` visibility.)

### 9.3 - Implementation-specified UB

The implementation is allowed to define new UB in situations where threads, OS access, or language extensions are involved. For example, it's OK for an implementation to define it to be UB to read and write to a single variable or memory location from two threads without using a synchronization primitive or memory fence, even if those accesses are direct or use correctly-derived pointers.

Implementations are allowed to specify things as being defined even if they're specified as UB here. For example, implementations are allowed to specify that it's not undefined for a variable's value to magically change after a memory fence or thread synchronization operation finishes (for example, if a pointer to that variable has passed into another thread).

Implementations are allowed to specify unaligned memory accesses as UB, but this is discouraged and it's strongly recommended that they specify them as implementation-defined instead.

## 10 - Non-features

Konoran is intended as a compiler target, and as such expects the "programmer" to provide a lot of its own runtime functionality.

Advanced implementations are highly encouraged to detect undefined behavior and produce warnings or errors.

### 10.1 - (No) memory management

Konoran does not include memory management tools; however, at runtime, konoran needs to be able to allocate new memory for function-local variables. A konoran compiler will probably put this memory on the stack, while an interpreter will probably put it on the heap.

Konoran implementations are encouraged to provide, or allow linking against, malloc/realloc/free-like functions, but the exact function signature or behavior of such functions is not specified here.

### 10.2 - (No) error handling

Konoran does not include any error handling tools. Konoran programmers are encouraged to return error codes through 'out' pointers, i.e. pointers to error metadata provided by the caller in a function argument.

### 10.3 - (No) standard library

Konoran's reference implementation includes a basic set of output-printing functions in JIT mode, for testing purpose. However, this is not a true standard library, and konoran does not provide a true standard library. Any standard library must be specified by and provided by the implementation if it is desired.

### 10.4 - (No) threading primitives

Konoran doesn't include anything related to threads, e.g. synchronization primitives, fences, etc. The implementation must specify and provide these things if they are desired.

### 10.5 - (No) string handling

Konoran supports utf-8 string literals, but they compile down to arrays or pointers over the `u8` type, not a unique string type. There is no string-handling library.

### 10.6 - Things that cause UB in C but not konoran

Konoran tries to avoid UB when it can. There are certain things C's spec does that make UB really easy to run into accidentally. Konoran tries not to do these things.

#### 10.6.1 - (No) strict aliasing analysis

Konoran **specifically does not have strict type-based aliasing rules** and implementations are explicitly not allowed to perform type-based pointer aliasing analysis. Aside from pointer provenance concerns, the "underlying bytes" beyond a pointer are not considered to have a type except for during the moment that they are accessed.

#### 10.6.2 - (No) forwards progress guarantee

C compilers are allowed to optimize C code with the assumption that no code path that leads to a non-terminating loop (sometimes with additional language like "observable behavior" depending who you ask) that has no side effects. C++'s more explicit version of this is the "forwards progress guarantee". Konoran does not make this assumption and even trivial infinite loops are allowed.

#### 10.6.3 - Certain things with conjured pointer values

Accessing conjured pointer values is UB in some situations in C if the compiler "knows" that the pointer doesn't point to an object. In konoran, any side effects from accessing "non-object" pointers is implementation-defined behavior instead of the operating being undefined, e.g. it can crash, give a bogus value, raise an exception, "have no effect on other objects", etc. But it can't be treated like it "didn't happen", even if the compiler knows that the pointer doesn't point to an object.

For example, if you manage to conjure a pointer with the same pointer value as the address of a local variable, and write to it without crashing, then read back the value you wrote via the conjured pointer, you "should" see the value you wrote to the conjured pointer, even if normal access to the variable itself is unaffected. [pointer_conjuration_test.knr](examples/pointer_conjuration_test.knr) is an example of this. Note the the pointer must **actually be conjured**. Doing pointer math to a correctly-derived pointer pointing at another variable is not pointer conjuration.

It's currently not possible for konoran to 100% guarantee this behavior while compiling down to LLVM, because LLVM's manual defines it as undefined behavior to access an object via a pointer that's not based on that object, but *in practice* it works.
