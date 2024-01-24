## Roadmap - Self-hosted compiler

- [X] Check correct implementation of a trait.
- [ ] `no_mangle` attribute to avoid procedure mangling.
- [ ] Format command (`rivet fmt .`).
- [ ] Replace Python scripts with Rivet scripts.
- [ ] Explicit procedure/method overloading:
    ```swift
    proc bool_to_string(b: bool) -> string { ... }
    proc int_to_string(i: int32) -> string { ... }

    proc to_string { bool_to_string, int_to_string }

    proc main() {
        @assert(to_string(true) == "true");
        @assert(to_string(10) == "10");
    }
    ```
- [ ] Constant-folding: `x := 2 * 2;` => `x := 4;`.
- [ ] Optimize code, inline procedures/methods annotated with `inline`, 
    delete unused code, etc.
- [ ] (Atomic) Reference-Counting for traits, tagged enums, strings, dynamic arrays and structs.
- [ ] Better support for embedded structs.
- [ ] `undefined` for uninitialized variables: `x: [5]uint8 := undefined;`.
- [ ] Add `@is_defined()` builtin procedure.
- [ ] Generic support: `Struct<T> { f: T; }` => `Struct:<T>(f: @default(T))`.
- [ ] Lambdas + Closures: `sum := |a: int32, b: int32| [my_inherited_var] a + b;`.
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

    proc xyz() { }
    ```
- [ ] Replace `c/libc` definitions by `extern (C) import` declarations.
