# Copyright (C) 2022 The Rivet Team. All rights reserved.
# Use of this source code is governed by an MIT license
# that can be found in the LICENSE file.

from lark import Lark
from lark.exceptions import UnexpectedInput, UnexpectedToken, UnexpectedCharacters

from .utils import eprint

Parser = Lark(
    r"""
module: declaration*

attrs: "@" "[" WORD (";" WORD)* "]"
declaration: attrs* (mod_decl | fn_decl)

mod_decl: "pub"? "mod" WORD "{" declaration* "}"
fn_decl: "pub"? "fn" WORD "(" ")" "{" "}"

COMMENT: "//" /[^\n]/*
MULTI_COMMENT: "/*" /.*/s "*/"

%import common.WORD
%import common.WS

%ignore WS
%ignore COMMENT
%ignore MULTI_COMMENT
""",
    start="module",
    parser="lalr",
)

def parse(filename: str):
    with open(filename, "r") as f:
        src= f.read()
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
