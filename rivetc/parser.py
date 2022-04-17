# Copyright (C) 2022 The Rivet Team. All rights reserved.
# Use of this source code is governed by an MIT license
# that can be found in the LICENSE file.

from lark import Lark, Transformer
from lark.exceptions import UnexpectedInput, UnexpectedToken, UnexpectedCharacters

from . import ast
from .utils import eprint

Parser = Lark(
    r"""
module: extern_pkg* (extern_decl | declaration)*

extern_pkg: "extern" "pkg" IDENT ";"

extern_decl: "extern" STRING (fn_without_body | "{" fn_without_body* "}")

declaration: mod_decl | fn_decl

attrs: "@" "[" IDENT (";" IDENT)* "]"

mod_decl: maybe_pub "mod" IDENT "{" declaration* "}"

fn_without_body: fn_header ";"
fn_decl: attrs* maybe_pub fn_header "{" "}"
fn_header: "fn" IDENT "(" (fn_arg ("," fn_arg)*)? ")" ("!"? type)?
fn_arg: "mut"? IDENT ":" type

!type: "?"? (IDENT | type_ptr | type_ref | type_array | type_slice)
type_ptr: "*" "*"* type
type_ref: "&" type
type_array: "[" type ";" NUMBER "]"
type_slice: "[" type "]"

!maybe_pub: "pub"?

// ------------------ Utils ------------------

// numbers
DIGIT: "0".."9"
HEXDIGIT: "a".."f"|"A".."F"|DIGIT

INT: DIGIT+
DECIMAL: INT "." INT? | "." INT

// floats
_EXP: ("e"|"E") ["+"|"-"] INT
FLOAT: ["+"|"-"] (INT _EXP | DECIMAL _EXP?)
// Khe xd, entonces se mi novia owo
NUMBER: FLOAT | INT

// strings
_STRING_INNER: /.*?/
_STRING_ESC_INNER: _STRING_INNER /(?<!\\)(\\\\)*?/
STRING: "\"" _STRING_ESC_INNER "\""

// identifiers
_LETTER: "A".."Z" | "a".."z"
IDENT: ("_" | _LETTER) ("_" | _LETTER | DIGIT)*

COMMENT: /\/\/[^\n]*/
MULTI_COMMENT: "/*" /(.|\n)*?/ "*/"

WS: /[ \t\f\r\n]/+

%ignore COMMENT
%ignore MULTI_COMMENT
%ignore WS
""",
    start="module",
    parser="lalr",
    cache=True,
)


def parse(filename: str, prefs, is_root=False):
    src = open(filename, "r").read()
    try:
        tree = Parser.parse(src)
        return AST_Transformer(prefs, is_root).transform(tree)
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

class AST_Transformer(Transformer):
    def __init__(self, prefs, is_root=False):
        self.prefs = prefs
        self.is_root = is_root

    def module(self, decls):
        return ast.SourceFile(self.prefs.pkg_name, decls, is_root=self.is_root)

    def extern_pkg(self, pkg):
        return ast.ExternPkg(pkg[0].name)

    def mod_decl(self, decls):
        is_pub = decls[0].data=="maybe_pub"
        print(is_pub)
        return ast.Mod(decls[1].name if is_pub else decls[0].name, decls[1:])

    # Terminals

    def IDENT(self, name):
        return ast.Ident(name)
