<div align="center">

<img src="https://github.com/rivet-lang/logo/blob/main/logo.png" alt="Rivet logo" width="200" height="200"/>

# The Rivet programming language

A general-purpose programming language, focused on simplicity, safety and stability.

[Website](https://rivet-lang.github.io)
•
[Documentation](https://rivet-lang.github.io/docs)
•
[Roadmap](ROADMAP.md)

![issues](https://img.shields.io/github/issues/rivet-lang/rivet?style=flat-square)
![status](https://img.shields.io/badge/status-alpha-blue?style=flat-square)
![license](https://img.shields.io/github/license/rivet-lang/rivet?style=flat-square)

</div>

Rivet's goal is to be a very powerful programming language and at the same time easy
to use, whose syntax is inspired by Go, Zig and C# and other programming languages.

Currently, Rivet uses C as the only backend and generates C99 code that is compiled using
a C compiler (by default, GCC or Clang). The idea is that in the long term there will be
other backends available for code generation, such as LLVM or WebAssembly, and also that
there will be an interpreter available.

You can find information on how to build Rivet on your computer by going to the
[documentation](https://rivet-lang.github.io/docs).

```v
import std.console;

fn main() {
    console.writeln("Hello World!");
}
```

> [!IMPORTANT]
> Currently the language is in alpha state, and therefore its syntax and the language
> API is not stable, and may change in the long term. Not all features are implemented.
> 
> Compiler version 0.1.0 will be released when the self-hosted compiler can compile itself
> successfully.
> 
> Only linux is supported for now. Windows is not well supported, and macOS is not supported
> yet. Any help to provide full support for both Windows and macOS is welcome.
> 
> Read [CONTRIBUTING](CONTRIBUTING.md) to get more information.

## Why?

There are many programming languages today, each specialized in a specific field or in
solving a certain problem.

Rivet is the materialization of my thinking about what a  perfect, secure, fast, readable,
powerful, simple and multi-platform programming language  would be like.

Rivet takes the best features of existing programming languages and tries  to unify them
into a single programming language, so that software development has the  best possible
quality.

## Features (WIP)

* **Easy-to-learn syntax**: A syntax without overload of unnecessary elements.
* **Named and Default arguments**: Very useful, `my_func(arg0 = 5)`.
* **Not NULL values by default**: This is only possible with option types (`?T`) and `none`.
* **Easy error handling**: With result types, `fn my_func() !T { ... }`,
    `throw` and `catch`.
* **Immutable values**: Variables and fields are immutable by default.
* **Polymorphism**: Traits, Embedded Records and Unions are supported.
* **Generics**: Specialize and reuse the same code for different types.

## Example: The classic `Hello World!`

```v
import std/console;

fn main() {
    console.log("Hello World!");
}
```

<!-- It is also possible to find valid code examples in the [`tests/valid`](tests/valid)
folder. -->
