<div align="center">

<img src="https://github.com/rivet-lang/logo/blob/main/logo.png" alt="Rivet logo" width="200" height="200"/>

# The Rivet programming language

A general-purpose programming language, focused on simplicity, safety and stability.

[Website](https://rivet-lang.github.io)
•
[Documentation](https://rivet-lang.github.io/docs)
•
[Roadmap](ROADMAP.md)
•
[Changelogs](changelogs/)
•
[Examples](examples/)

![issues](https://img.shields.io/github/issues/rivet-lang/rivet?style=flat-square)
![status](https://img.shields.io/badge/status-alpha-blue?style=flat-square)
![license](https://img.shields.io/github/license/rivet-lang/rivet?style=flat-square)

[![](https://dcbadge.vercel.app/api/server/thCA4VsWgs)](https://discord.gg/thCA4VsWgs)

</div>

Rivet's goal is to be a very powerful programming language and at the same time easy
to use, with a syntax that is the result of mixing Go + Zig + C# and other languages
such as Python, Lua, TypeScript, D, Swift, etc.

Currently, Rivet uses C as the only backend and generates C99 code that is compiled using
a C compiler (by default, gcc or clang). The idea is that in the long term there will be
other backends available for code generation, such as LLVM or WebAssembly, and also that
there will be an interpreter available.

You can find information on how to build Rivet on your computer by going to the
[documentation](https://rivet-lang.github.io/docs).

## Why?

There are many programming languages today, each specialized in a specific field or in
solving a certain problem.

Rivet is the materialization of my thinking about what a  perfect, secure, fast, readable,
powerful, simple and multi-platform programming language  would be like.

Rivet takes the best features of existing programming languages and tries  to unify them
into a single programming language, so that software development has the  best possible
quality.

## Features

* **Easy-to-learn syntax**: A syntax without overload of unnecessary elements.
* **Named and optional arguments**: Very useful, `my_func(arg0: 5)`.
* **Not NULL values by default**: This is only possible with option types (`?T`) and `none`.
* **Easy error handling**: With result types, `func my_func() -> !T { ... }`,
    `throw` and `catch`.
* **A basic preprocessor**: `if`, `else_if`, `else` and `endif` for optional code using
    flags (`-D my_flag`).
* **Immutable values**: Variables and fields are immutable by default.
    structs have internal immutability.
* **Polymorphism**: Traits, Embedded Structs and Tagged Enums are supported.
* **Generics**: Specialize and reuse the same code for different types (**coming soon**).

More features that will be added in the self-hosted compiler can be seen in the [roadmap](ROADMAP.md).

## Example: The classic `Hello World!`

```swift
import std/console;

func main() {
    console.writeln("Hello World!");
}
```
More examples in the [`examples/`](examples/) folder.
It is also possible to find valid code examples in the  [`tests/valid`](tests/valid)
folder.

## Important note

Currently the language is in alpha state, and therefore its syntax and the language
API is not stable, and may change in the long term. Not all features are implemented.

Compiler version 0.1.0 will be released when the self-hosted compiler can compile itself
successfully.

Only linux is supported for now. Windows is not well supported, and macOS is not supported
yet. Any help to provide full support for both Windows and macOS is welcome.

Read [CONTRIBUTING](CONTRIBUTING.md) to get more information.