# Code Structure

## Comments
```rust
// This is a single line comment.
/*
This is a multiline comment.
*/
```

You can use comments to make reminders, notes, or similar things in your
code.

## Entry point

In Rivet, the entry point of a program is a function named `main`.
```rust
fn main() void {
    // code goes here
}
```

## Top-level declarations

On the top level only declarations are allowed.
```rust
use pkg::foo;

mod Foo { /* ... */ }

const Foo: i32 = 0;

let Foo: i32 = 0;

type Foo = i32;

trait Foo { /* ... */ }

struct Foo { /* ... */ }

enum Foo { /* ... */ }

extend Foo { /* ... */ }

fn baz() { /* ... */ }

test "test name =D" { /* ... */ }
```

* * *

<div align="center">

[back](00_getting_started.md) **|** [next](02_functions.md)

</div>
