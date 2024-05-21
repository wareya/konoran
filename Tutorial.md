# Konoran Tutorial

This tutorial is meant for people who already have a passive knowledge of the C programming language. It may still make sense if you don't, because konoran is a very simple language, but if you don't know C and it's confusing, it's not your fault.

This is WIP and I'm adding more sections as I feel like it.

## 1 - A basic program

Let's write a konoran program!

```php
void main()
{
    return;
}
```

This looks a lot like C, which is konoran's main inspiration.

But if you look very closely, there's something subtle: We *must* explicitly return from this function with a return statement, despite the function returning `void` (no value). We cannot leave it blank and have it automatically return value. In konoran, all functions must explicitly return or enter into an infinite loop in all control paths.

## 2 - An infinite loop

Let's make an infinite loop:

```php
void infinite_loop()
{
loop_head:
    goto loop_head;
}
```

Two things to note here:

1) Konoran does not have looping constructs like 'while' or 'for'. The only way to loop is to use a goto. While overusing gotos is bad in high-level languages, konoran is not a high-level language; it's a "compiler target" language, just one that looks and feels like high-level code.
2) In modern versions of C and C++, trivial infinite loops are "undefined behavior". In konoran, they are not. Konoran tries to avoid undefined behavior where it can, as its design philosophy prioritizes ease-of-understanding over most things.

# 3 - A real program

Now, let's go back to our `main` function and try interpreting the command line arguments. Let's write a program that looks at the first command line argument (other than the zeroth argument, which is the command being executed) and interprets it as a number, then prints that number:

```php
i32 main(i32 argc, ptr(ptr(u8)) argv)
{
    if (argc <= 1i32)
    {
        return -1i32;
    }
    
    u64 n = 0u64;
    ptr(u8) c = *(argv + sizeof ptr(u8));

    loop_head:
    if (*c != 0u8)
    {
        n = (n * 10u64) + (*c - '0') as u64;
        c = c + 1u64;
        goto loop_head;
    }
    
    print_float((n) as f64);
    return 0i32;
}
```

This code interprets the second argument as a string of ASCII number digits from 0 to 9, with no other characters expected, and parses them as a u64.

Quite a few things to note here:

1) The syntax for pointer declarations is different from C, but not the syntax for their access. This syntax is supposed to be harder to misunderstand and easier to machine-generate.
2) All number type names explicitly encode their number of bits and whether they're signed or not: u8, i32, u64, etc.
3) Number literals have their type attached as a suffix (like in 0i32).
4) We have a block-sized if statement, rather than *just* a conditional goto command. (We do also have a conditional goto statement, but it's not used here.)
5) If statement bodies have to be surrounded by braces, even if they're only a single line wrong. This is not intended to prevent bugs in human-readable code; rather, it differentiates normal if statements from conditional gotos, which look like `if (expr) goto label;` without braces.
6) There aren't any compound assignment operators like `+=`.
7) Casting takes the form `(expr) as <type>` rather than the `(<type>) expr` that it takes in C. This is to make it look visually distinct from normal expressions, and to make it easier to ctrl+f for casts. I adopted the idea of an `as` operator from Rust.
8) There are no automatic conversions, not even those that are lossless like `u8 -> u64`. Converted values must be explicitly casted. This is not meant to prevent bugs in human-written code; it is meant to ensure that machine-generated code is being consistent and understands konoran's type system.

The `print_float` function doesn't come from anywhere; konoran does not have a standard library. Rather, in JIT mode, the konoran compiler provides a couple of helper functions for debugging. In compile-to-disk mode, if you want to use such functions, they need to be provided by a separate object file linked in by a separate compiler. (Konoran does not include a linker.)

