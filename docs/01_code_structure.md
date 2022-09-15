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
fn main() {
    // code goes here
}
```

## Top-level declarations

On the top level only declarations are allowed.
```rust
using pkg::module;

mod M { /* ... */ }

var S: i32 = 0;

const C: i32 = 0;

type Foo = i32;

trait Foo { /* ... */ }

union Foo { /* ... */ }

struct Foo { /* ... */ }

enum Foo { /* ... */ }

extend Foo { /* ... */ }

fn baz() { /* ... */ }
```

* * *

<div align="center">

[back](00_getting_started.md) **|** [next](02_functions.md)

</div>
