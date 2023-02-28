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
[TODO](TODO.md)
•
[Changelog](CHANGELOG.md)
•
[Samples](samples/)

![issues](https://img.shields.io/github/issues/rivet-lang/rivet?style=flat-square)
![status](https://img.shields.io/badge/status-alpha-blue?style=flat-square)
![license](https://img.shields.io/github/license/rivet-lang/rivet?style=flat-square)

[![](https://dcbadge.vercel.app/api/server/thCA4VsWgs)](https://discord.gg/thCA4VsWgs)

</div>

Rivet's goal is to be a very powerful programming language and at the same time easy
to use, with a syntax that is the result of mixing Go + Zig + C# and other languages
such as Python, Lua, TypeScript, D, etc.

## Why?

It is true that there are many programming languages and each of them specialized in
a field; some with strange syntax in my opinion (like Lisp) and others with a high
learning curve, but Rivet tries to be an exception by trying to become a unique
language with a clear and simple syntax and a low learning curve.

## Important note

Currently the language is in alpha state, and therefore its syntax and the language
API is not stable, and may change in the long term. Not all features are implemented.

Only linux is supported. Windows is not well supported, and macOS is not supported yet.
Any help is welcome.

## Features

* **Easy-to-learn syntax**: A syntax without overload of unnecessary elements.
* **Easy error handling**: Via result types: `func my_func() !T { ... }`.
* **Not NULL values by default**: This is only possible with optional pointers.
* **Named and optional arguments**: Very useful.
* **Immutable values**: Variables and fields are immutable by default.
* **Polymorphism**: Traits and Embedded Structs are supported.
* **Generics**: Specialize and reuse the same code for different types (**coming soon**).

## Samples

```swift
// A simple sample: the classic "Hello World"
import "std/console";

func main() {
    console.println("Hello World!");
}
```
More samples in the [`samples/`](samples/) folder.

<div align="center">

© 2023 **The Rivet Developers**

</div>
