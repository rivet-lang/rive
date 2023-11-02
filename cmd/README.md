# The Rivet command-line interface

This is the Rivet command-line interface, the compiler source code is
located in the [`lib/rivet`](../lib/rivet) directory.

This is the entry point of the compiler. In addition to this, also in 
this module are the source codes for the subcommands that the compiler 
has.

**NOTE:** The `build`, `run`, `test` and `check` subcommands are 
handled by the compiler directly, and their source code can be found 
in the `rivet` module.
