## Python3 bootstrap-compiler

[X] support `import { self, File } from "std/fs"`.
[X] fix `if let x = result_fn() { ... }`.
[X] fix `+=` operators with overloaded operators.
[X] support virtual methods and class vtables.
[X] implement `switch 1 { 2 if abc => { ... } }`.
[X] implement `switch advance_enum_obj is { .Value as val => { ... } }`.
[X] check advance enums casts.
[X] `@vec` builtin function.
[X] mutable arrays/vectors: `[]mut T`/`[SIZE]mut T`.
[X] `for &v in iterable {` and `for mut v in iterable`.

## Self-hosted compiler

[ ] add `HashMap<K, V>`.
[ ] constant-folding.
[ ] optimize code.
[ ] function literals.
[ ] anonymous structs/classes.
[ ] better support for embedded structs.
[ ] abstract classes and methods.
[ ] reference counting for classes, traits, advance enums, strings and vectors.
[ ] do not modify the values of primitive types passed as an argument that is 
declared mutable (this should only work with reference types):
    ```ri
    fn arg(mut x: i32) {
        x += 1;
    }

    let y = 1;
    arg(y); // `y` should not be modified, and the compiler should not require 
            // it to be a mutable variable.
    ```
