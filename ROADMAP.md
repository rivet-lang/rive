## Roadmap - Self-hosted compiler

- [X] Check correct implementation of a trait.
- [ ] Explicit function/method overloading:
    ```swift
    func bool_to_string(b: bool) -> string {...}
    func int_to_string(i: int32) -> string {...}

    func to_string { bool_to_string, int_to_string }

    func main() {
        @assert(to_string(true) == "true");
        @assert(to_string(10) == "10");
    }
    ```
- [ ] Constant-folding: `x := 2 * 2;` => `x := 4;`.
- [ ] Optimize code.
- [ ] (Atomic) Reference-Counting for traits, boxed enums, strings and vectors.
- [ ] Btter support for embedded structs.
- [ ] `undefined` for uninitialized variables: `x: [5]uint8 := undefined;`.
- [ ] Disallow empty array literal (`x := []!; -> ERROR`).
- [ ] Add `@is_flag_defined()` builtin function.
- [ ] Generic support: `Struct<T> { f: T; }` => `Struct:<T>(f: @default(T))`.
- [ ] Function literals: `sum := func(a: int32, b: in32) int32 { return a + b; };`.
- [ ] Anonymous structs: `my_obj := struct(my_field: 1, other_field: true, pwd: "abc");`.
- [ ] `extern (C) import` for C interop:
    ```swift
    extern (C) import {
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
- [ ] Replace `c/libc` definitions by `extern (C) import` declarations.
