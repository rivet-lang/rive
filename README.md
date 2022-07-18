<div align="center">

<img src="docs/assets/logo.png" alt="Rivet logo" width="200" height="200"/>

# The Rivet programming language

![issues](https://img.shields.io/github/issues/rivet-lang/rivet?style=flat-square)
![status](https://img.shields.io/badge/status-alpha-blue?style=flat-square)
![license](https://img.shields.io/github/license/rivet-lang/rivet?style=flat-square)

[Documentation](docs/00_getting_started.md)

<!--
•
[Changelog](CHANGELOG.md)
-->

A general-purpose programming language, focused on simplicity, safety and stability.

</div>

> **NOTE:** Rivet is still in the development phase.

Rivet's goal is to be a very powerful programming language and at the same time easy
to use, with a syntax inspired mainly by Rust and C# (which are the coolest languages
I've ever seen), and by other languages such as Python, Lua, TypeScript, D, etc.

It is true that there are many programming languages and each of them specialized in
a field; some with strange syntax in my opinion (like Lisp) and others with a high
learning curve, but Rivet tries to be an exception by trying to become a unique
language with a clear and simple syntax and a low learning curve.

## Basic features

* [X] **Easy-to-learn syntax**: A syntax without overload of unnecessary elements.
* [X] **Easy error handling**: Via result types: `fn func() !T { ... }`.
* [X] **Named and optional arguments**: Very useful.
* [X] **Immutable values by default:** Variables, arguments and fields of struct
are immutable by default.
* [ ] **Ownership and Borrowing**: Rivet has a very simplified version of Rust's
Ownership and Borrowing system. The lifetimes are inferred by the compiler.
* [ ] **Struct inheritance and polymorphism**: Trait and Union types are also
supported.
* [ ] **Generics:** Specialize and reuse the same code for different types.

## Roadmap
* [ ] Make lib basically available
* [ ] Make self hosted compiler

## Dependencies

The Rivet compiler currently generates C code, so a C compiler, which supports C11,
is required to generate executables. Over time the compiler will add support for
generating binaries directly without the need for a C compiler.

## Run the compiler

> **NOTE:** To run the compiler you must have Python 3.

> **NOTE:** At present, the test is successful only on **Linux** and **Windows**.

Just execute `python rivetc.py xxx.ri`.

* * *

<div align="center">

Copyright © 2022 **The Rivet Team**

</div>
