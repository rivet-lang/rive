<div align="center">

<img src="doc/assets/logo.png" alt="Rivet logo" width="200" height="200"/>

# The Rivet programming language

A general-purpose programming language, focused on simplicity, safety and stability.

[Documentation](doc/00_getting_started.md)
•
[Roadmap](ROADMAP.md)
•
[Changelog](CHANGELOG.md)
•
[Samples](samples/)

![issues](https://img.shields.io/github/issues/rivet-lang/rivet?style=flat-square)
![status](https://img.shields.io/badge/status-alpha-blue?style=flat-square)
![license](https://img.shields.io/github/license/rivet-lang/rivet?style=flat-square)

</div>

Rivet's goal is to be a very powerful programming language and at the same time easy
to use, with a syntax inspired mainly by Rust and C# (which are the coolest languages
I've ever seen), and by other languages such as Python, Lua, TypeScript, D, Zig, Go,
etc.

It is true that there are many programming languages and each of them specialized in
a field; some with strange syntax in my opinion (like Lisp) and others with a high
learning curve, but Rivet tries to be an exception by trying to become a unique
language with a clear and simple syntax and a low learning curve.

```rs
// A simple sample: the classic "Hello World"
import "std/console";

fn main() {
    console.println("Hello World!");
}
```
<sub>More samples in the [`samples/`](samples/) folder</sub>

## Important note

Currently the language is in alpha state, and therefore its syntax and the language
API is not stable, and may change in the long term. Not all features are implemented.

Only linux is supported. Windows is not well supported, and macOS is not supported yet.
Any help is welcome.

## Features

* **Easy-to-learn syntax**: A syntax without overload of unnecessary elements.
* **Easy error handling**: Via result types: `fn my_fn() !T { ... }`.
* **Not NULL values by default**: This is only possible with optional pointers.
* **Named and optional arguments**: Very useful.
* **Immutable values**: Variables and fields of struct are immutable by default.
* **Polymorphism**: Classes, Traits and Embedded Structs are supported.
* **Generics**: Specialize and reuse the same code for different types (**coming soon**).

* * *

<div align="center">

© 2022 **The Rivet Team**

</div>
