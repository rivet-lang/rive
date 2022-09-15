# Statements

Each statement must end with a semicolon.

## Variables

Variables are like boxes that contain values.

Variables are declared as follows: `var [mut] <name>[: <type>] = <value>;`.
Example:

```rust
var x: i32 = 1;
```

We have created a variable called `x`, which contains the value 1 and is
of type `i32`.

The type of the variable can be omitted.

```rust
var x = 1; // via inference, the compiler knows that `x` is an `i32`.
```

By default, all variables are immutable, that is, their values do not change.
To change the value of a variable you have to declare it with `mut`.

```rust
var mut x = 1;
x = 2; // this is valid

var y = 1;
y = 2; // error: `y` is immutable
```

Multiple values can be assigned on a single line via tuple-destructuring, example:

```rust
var (a, b, c) = (1, 2, 3);
var (c: i32, d: i32, e: i32) = (4, 5, 6);
var (f, g, h) = tuple_fn();

// this is a short form for:

var a = 1;
var b = 2;
var c = 3;

var c: i32 = 4;
var d: i32 = 5;
var e: i32 = 6;

var tmp_tuple_fn = tuple_fn();
var f = tmp_tuple_fn.0;
var g = tmp_tuple_fn.1;
var h = tmp_tuple_fn.2;
```

* * *

<div align="center">

[back](02_functions.md) **|** [next](03_statements.md)

</div>
