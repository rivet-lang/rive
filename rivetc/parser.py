# Copyright (C) 2022 The Rivet Team. All rights reserved.
# Use of this source code is governed by an MIT license
# that can be found in the LICENSE file.

from lark import Lark
from lark.exceptions import UnexpectedInput, UnexpectedToken, UnexpectedCharacters

from .utils import eprint

Parser = Lark(
    r"""
module: extern_pkg* (extern_decl | declaration)*

extern_pkg: "extern" "pkg" IDENT ";"

extern_decl: "extern" STRING (fn_without_body | "{" fn_without_body* "}")

declaration: mod_decl | fn_decl

attrs: "@" "[" IDENT (";" IDENT)* "]"

mod_decl:  "pub"? "mod" IDENT "{" declaration* "}"

fn_without_body: fn_header ";"
fn_decl:  attrs* "pub"? fn_header "{" "}"
fn_header: "fn" IDENT "(" (fn_arg ("," fn_arg)*)? ")" ("!"? type)?
fn_arg: "mut"? IDENT ":" type

type: "?"? (IDENT | "&" type | "[" type (";" NUMBER )? "]")

// ------------------ Utils ------------------

// numbers
DIGIT: "0".."9"
HEXDIGIT: "a".."f"|"A".."F"|DIGIT

INT: DIGIT+
DECIMAL: INT "." INT? | "." INT

// floats
_EXP: ("e"|"E") ["+"|"-"] INT
FLOAT: ["+"|"-"] (INT _EXP | DECIMAL _EXP?)

NUMBER: FLOAT | INT

// strings
_STRING_INNER: /.*?/
_STRING_ESC_INNER: _STRING_INNER /(?<!\\)(\\\\)*?/
STRING: "\"" _STRING_ESC_INNER "\""

// identifiers
_LETTER: "A".."Z" | "a".."z"
IDENT: ("_" | _LETTER) ("_" | _LETTER | DIGIT)*

COMMENT: /\/\/[^\n]*/
MULTI_COMMENT: "/*" /.*/s "*/"

WS: /[ \t\f\r\n]/+

%ignore COMMENT
%ignore MULTI_COMMENT
%ignore WS
""",
    start="module",
    parser="lalr",
    cache=True,
)


def parse(filename: str):
    src = open(filename, "r").read()
    try:
        return Parser.parse(src)
    except UnexpectedInput as err:
        ctx = err.get_context(src).strip()
        start = f"{filename}:{err.line}:{err.column}: error:"
        if isinstance(err, UnexpectedToken):
            eprint(f"{start} unexpected token {err.token}")
            eprint(ctx)
            eprint(f"expected one of: {err.expected}")
        elif isinstance(err, UnexpectedCharacters):
            eprint(f"{start} unexpected character")
            eprint(ctx)
        else:
            eprint(f"{start} unexpected end of file")
            eprint(ctx)
            eprint(f"expected one of: {err.expected}")
        exit(1)
