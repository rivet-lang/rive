<div align="center">
<!--
<img src="docs/assets/logo.png" alt="Rivet logo" width="200" height="200"/>
-->

# The Rivet programming language

![issues](https://img.shields.io/github/issues/rivet-lang/rivet?style=flat-square)
![status](https://img.shields.io/badge/status-alpha-blue?style=flat-square)
![license](https://img.shields.io/github/license/rivet-lang/rivet?style=flat-square)

<!--
[Docs](docs/docs.md) •
[Changelog](CHANGELOG.md)
-->

A general-purpose programming language, focused on simplicity, safety and stability.

</div>

> **NOTE:** Rivet is still in the development phase.

Rivet's goal is to be a very powerful programming language and at the same time easy
to use, with a syntax inspired mainly by Rust and C# (which are the coolest languages
I've ever seen), and by other languages such as Python, Lua, TypeScript, etc.

## Features

* **Easy-to-learn syntax**: A syntax without overload of unnecessary elements.
* **Easy error handling**: Via result types: `fn alloc() !`.
* **Named and optional arguments**: Very useful.
* **Struct inheritance and polymorphism**: Traits and Tagged Unions are also supported.
* **Ownership and Borrowing**: Rivet has a very simplified version of Rust's
Ownership and Borrowing system. The lifetimes are inferred by the compiler.

## Run the compiler

> **NOTE:** To run the compiler you must have Python 3.

Just execute `python3 rivetc.py`.

* * *

<div align="center">

Copyright (C) 2022 **The Rivet Team**

</div>
