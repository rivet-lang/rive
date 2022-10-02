# Functions

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

A function body is made up of 1 or more statements and can be empty.

```rust
fn x() { /* empty body */ }
fn y() {
	let my_var = 1; // statement
}
```

## Arguments

The arguments are declared as follows: `<name>: <type> [= default_value]`,
for example: `arg1: i32`, `arg2: bool = false`.

The arguments are immutable.

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

* * *

<div align="center">

[back](01_code_structure.md) **|** [next](03_statements.md)

</div>
