# Statements

Each statement must end with a semicolon.

## Variables

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
let (f, g, h) = tuple_func();

// this is a short form for:

let a = 1;
let b = 2;
let c = 3;

let c: i32 = 4;
let d: i32 = 5;
let e: i32 = 6;

let tmp_tuple_func = tuple_func();
let f = tmp_tuple_func.0;
let g = tmp_tuple_func.1;
let h = tmp_tuple_func.2;
```

* * *

<div align="center">

[back](02_functions.md) **|** [next](03_statements.md)

</div>
