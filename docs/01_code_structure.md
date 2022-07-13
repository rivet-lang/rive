# Code Structure

In Rivet, the entry point of a program is a function named `main`.
```rust
fn main() {
    // code goes here
}
```

On the top level only declarations are allowed.
```rust
using pkg::module;

const C: i32 = 0;

static S: i32 = 0;

type Foo = i32;

trait Foo {}

union Foo {}

struct Foo {}

enum Foo {}

extend Foo {}

fn baz() {}
```

## Comments
```rust
// This is a single line comment.
/*
This is a multiline comment.
*/
```

You can use comments to make reminders, notes, or similar things in your
code.

## Functions

Functions contain a series of arguments, a return type, and a body with
multiple statements.

The way to declare functions in Rivet is as follows:

```rust
fn <name>(<args>) [return_type] {
	...
}
```

For example:

```rust
fn add(a: i32, b: i32) i32 {
	return a + b;
}
```

`add` returns the result of adding the arguments `a` and `b`.

Functions can omit the return type if they return nothing, as well as have
0 arguments.

```rust
// `f1` returns a simple numeric value of type `i32`.
fn f1() i32 {
	return 0;
}

// `f2` takes an argument of type `i32` and prints it to the console.
fn f2(a: i32) {
	println("a: {}", a);
}

// `f3` takes no arguments and returns void.
fn f3() { }
```

### Arguments

The arguments are declared as follows: `<name>: <type> [= default_value]`,
for example: `arg1: i32`, `arg2: bool = false`.

They can also have default values, this bypasses the need to pass the
argument each time the function is called: `arg1: i32 = 5`.

So, if we have a function called `f5` with a default value argument,
we can call it in 3 ways:

```rust
fn f5(arg1: i32 = 5) {
	println("arg1: {}", arg1);
}

f5(); // use the default value `5`
f5(100); // will print 100 instead of 5 to the console

// this uses a feature called `named argument`, which allows an optional
// argument to be given a value by its name in any order
f5(arg1: 500); // will print 500 instead of 5 to the console
```

## Statements

### Variables

Variables are like boxes that contain values.

Variables are declared as follows: `let [mut] <name>[: <type>] = <value>;`.
Example:

```rust
let x: i32 = 1;
```

We have created a variable called `x`, which contains the value 1 and is
of type `i32`.

The type of the variable can be omitted.

```rust
let x = 1; // via inference, the compiler knows that `x` is an `i32`.
```

By default, all variables are immutable, that is, their values do not change.
To change the value of a variable you have to declare it with `mut`.

```rust
let mut x = 1;
x = 2; // this is valid

let y = 1;
y = 2; // error: `y` is immutable
```

Multiple values can be assigned on a single line via tuple-destructuring, example:

```rust
let (a, b, c) = (1, 2, 3);
let (c: i32, d: i32, e: i32) = (4, 5, 6);

// this is a short form for:

let a = 1;
let b = 2;
let c = 3;

let c: i32 = 4;
let d: i32 = 5;
let e: i32 = 6;
```

* * *

<div align="center">

[back](00_getting_started.md) **|** [next](01_code_structure.md)

</div>
