## Python3 bootstrap-compiler

- [X] support `import { self, File } from "std/fs"`.
- [X] fix `if x := result_fn() { ... }`.
- [X] fix `+=` operators with overloaded operators.
- [X] implement `switch 1 { 2 if abc => { ... } }`.
- [X] implement `switch advance_enum_obj is { .Value as val => { ... } }`.
- [X] check advance enums casts.
- [X] `@vec` builtin function.
- [X] mutable arrays/vectors: `[]mut T`/`[SIZE]mut T`.
- [X] `for &v in iterable {` and `for mut v in iterable`.

## Self-hosted compiler

- [ ] add `HashMap<K, V>`.
- [ ] constant-folding.
- [ ] optimize code.
- [ ] function literals.
- [ ] anonymous structs.
- [ ] better support for embedded structs.
- [ ] reference counting for traits, advance enums, strings and vectors.
- [ ] `undefined` for uninitialized variables.
- [ ] disallow empty array literal (`x := []!; -> ERROR`).
- [ ] check correct implementation of a trait.
- [ ] disallow use of references outside of functions.
- [ ] add `@is_flag_defined()` builtin function.
