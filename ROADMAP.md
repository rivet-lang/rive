# Roadmap

## Python3 bootstrap-compiler

- [X] support `import { self, File } from std/fs`.
- [X] implement `switch 1 { 2 if abc => { ... } }`.
- [X] implement `switch boxed_enum_obj is { .Value as val => { ... } }`.
- [X] check boxed enums casts.
- [X] `@vec` builtin function.
- [X] mutable arrays/vectors: `[]mut T`/`[SIZE]mut T`.
- [X] `for &v in iterable` and `for mut v in iterable`.

## Self-hosted compiler

- [X] check correct implementation of a trait.
- [ ] constant-folding: `x := 2 * 2;` => `x := 4;`.
- [ ] optimize code.
- [ ] reference counting for traits, boxed enums, strings and vectors.
- [ ] better support for embedded structs.
- [ ] `undefined` for uninitialized variables: `x: [5]uint8 := undefined;`.
- [ ] disallow empty array literal (`x := []!; -> ERROR`).
- [ ] add `@is_flag_defined()` builtin function.
- [ ] generic support: `Struct![T] { f: T; }` => `Struct![T](f: @default(T))`.
- [ ] add `std.collections.HashMap![K, V]`.
- [ ] function literals: `sum := func(a: int32, b: in32) int32 { return a + b; };`.
- [ ] anonymous structs: `my_obj := struct(my_field: 1, other_field: true, pwd: "abc");`.
- [ ] `c_import` for C interop:
    ```swift
    c_import {
        include <stdio.h>
        define MY_FLAG
        include "../header.h"
        if MY_FLAG_FROM_C {
            include_dir "../my_c_dir/"
        }
    }

    func xyz() { }
    ```
- [ ] `comptime` statement for compile-time execution (`CTE`):
    ```swift
    comptime {
        my_func();
    }

    func my_func() {
        console.println("executed in compile-time");
    }
    ```
- [ ] Replace `c/libc` definitions by `c_import` declarations.
