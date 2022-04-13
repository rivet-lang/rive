# Copyright (C) 2022 The Rivet Team. All rights reserved.
# Use of this source code is governed by an MIT license
# that can be found in the LICENSE file.

from lark import Lark

from .utils import eprint

Parser = Lark(
    r"""
module: declaration*

declaration: mod_decl | fn_decl

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
    try:
        return Parser.parse(open(filename, "r").read())
    except Exception as err:
        # Currently the error msgs are ugly, but they work!
        eprint(f"{filename}:{err.line}:{err.column}: error: {err}")
        exit(1)
