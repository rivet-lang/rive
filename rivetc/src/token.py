# Copyright (C) 2022 The Rivet Team. All rights reserved.
# Use of this source code is governed by an MIT license
# that can be found in the LICENSE file.

from enum import IntEnum as Enum, auto as auto_enum

class Kind(Enum):
    Unknown = auto_enum() # unknown
    EOF = auto_enum() # end of file
    DocComment = auto_enum() # doc-comment
    Name = auto_enum() # name
    Number = auto_enum() # number
    Char = auto_enum() # character
    String = auto_enum() # string
    Plus = auto_enum() # +
    Minus = auto_enum() # -
    Mul = auto_enum() # *
    Div = auto_enum() # /
    Mod = auto_enum() # %
    Assign = auto_enum() # =
    PlusAssign = auto_enum() # +=
    MinusAssign = auto_enum() # -=
    MulAssign = auto_enum() # *=
    DivAssign = auto_enum() # /=
    ModAssign = auto_enum() # %=
    AmpAssign = auto_enum() # &=
    PipeAssign = auto_enum() # |=
    XorAssign = auto_enum() # ^=
    Eq = auto_enum() # ==
    Ne = auto_enum() # !=
    Lt = auto_enum() # <
    Gt = auto_enum() # >
    Le = auto_enum() # <=
    Ge = auto_enum() # >=
    Lshift = auto_enum() # <<
    Rshift = auto_enum() # >>
    Dot = auto_enum() # .
    DotDot = auto_enum() # ..
    Ellipsis = auto_enum() # ...
    Arrow = auto_enum() # =>
    Comma = auto_enum() # ,
    Colon = auto_enum() # :
    DoubleColon = auto_enum() # ::
    Semicolon = auto_enum() # ;
    Question = auto_enum() # ?
    OrElse = auto_enum() # ??
    Bang = auto_enum() # !
    Amp = auto_enum() # &
    Pipe = auto_enum() # |
    Xor = auto_enum() # ^
    BitNot = auto_enum() # ~
    Hash = auto_enum() # #
    Dollar = auto_enum() # $
    Lbrace = auto_enum() # {
    Rbrace = auto_enum() # }
    Lbracket = auto_enum() # [
    Rbracket = auto_enum() # ]
    Lparen = auto_enum() # (
    Rparen = auto_enum() # )

    KeywordBegin = auto_enum()
    # ========== keywords ==========
    KwAnd = auto_enum() # and
    KwAs = auto_enum() # as
    KwBase = auto_enum() # base
    KwBreak = auto_enum() # break
    KwCatch = auto_enum() # catch
    KwClass = auto_enum() # class
    KwConst = auto_enum() # const
    KwContinue = auto_enum() # continue
    KwDefer = auto_enum() # defer
    KwElse = auto_enum() # else
    KwEnum = auto_enum() # enum
    KwErrDefer = auto_enum() # errdefer
    KwExtend = auto_enum() # extend
    KwExtern = auto_enum() # extern
    KwFalse = auto_enum() # false
    KwFor = auto_enum() # for
    KwFn = auto_enum() # fn
    KwIf = auto_enum() # if
    KwImport = auto_enum() # import
    KwIn = auto_enum() # in
    KwIs = auto_enum() # is
    KwLet = auto_enum() # let
    KwMut = auto_enum() # mut
    KwNil = auto_enum() # nil
    KwNotIn = auto_enum() # !in
    KwNotIs = auto_enum() # !is
    KwOr = auto_enum() # or
    KwPub = auto_enum() # pub
    KwProt = auto_enum() # prot
    KwReturn = auto_enum() # return
    KwSelf = auto_enum() # self
    KwSelfTy = auto_enum() # Self
    KwStruct = auto_enum() # struct
    KwSwitch = auto_enum() # switch
    KwTest = auto_enum() # test
    KwTrait = auto_enum() # trait
    KwTrue = auto_enum() # true
    KwType = auto_enum() # type
    KwUnsafe = auto_enum() # unsafe
    KwWhile = auto_enum() # while
    # ==============================

    KeywordEnd = auto_enum()

    def is_start_of_type(self):
        return self in (
            Kind.Bang, Kind.Name, Kind.Lparen, Kind.Amp, Kind.Mul,
            Kind.Lbracket, Kind.Question, Kind.KwSelf, Kind.KwSelfTy
        )

    def is_assign(self):
        return self in (
            Kind.Assign, Kind.PlusAssign, Kind.MinusAssign, Kind.MulAssign,
            Kind.DivAssign, Kind.ModAssign, Kind.AmpAssign, Kind.PipeAssign,
            Kind.XorAssign,
        )

    def is_relational(self):
        return self in (
            Kind.Eq, Kind.Ne, Kind.Lt, Kind.Gt, Kind.Le, Kind.Ge, Kind.KwIs,
            Kind.KwNotIs, Kind.KwIn, Kind.KwNotIn
        )

    def is_overloadable_op(self):
        return self in OVERLOADABLE_OPERATORS

    def __repr__(self):
        return TOKEN_STRINGS[self] if self in TOKEN_STRINGS else "unknown"

    def __str__(self):
        return self.__repr__()

TOKEN_STRINGS = {
    Kind.Unknown: "unknown",
    Kind.EOF: "end of file",
    Kind.DocComment: "documentation comment",
    Kind.Name: "name",
    Kind.Number: "number",
    Kind.Char: "character",
    Kind.String: "string",
    Kind.Plus: "+",
    Kind.Minus: "-",
    Kind.Mul: "*",
    Kind.Div: "/",
    Kind.Mod: "%",
    Kind.Assign: "=",
    Kind.PlusAssign: "+=",
    Kind.MinusAssign: "-=",
    Kind.MulAssign: "*=",
    Kind.DivAssign: "/=",
    Kind.ModAssign: "%=",
    Kind.AmpAssign: "&=",
    Kind.PipeAssign: "|=",
    Kind.XorAssign: "^=",
    Kind.Eq: "==",
    Kind.Ne: "!=",
    Kind.Lt: "<",
    Kind.Gt: ">",
    Kind.Le: "<=",
    Kind.Ge: ">=",
    Kind.Lshift: "<<",
    Kind.Rshift: ">>",
    Kind.Dot: ".",
    Kind.DotDot: "..",
    Kind.Ellipsis: "...",
    Kind.Arrow: "=>",
    Kind.Comma: ",",
    Kind.Colon: ":",
    Kind.DoubleColon: "::",
    Kind.Semicolon: ";",
    Kind.Question: "?",
    Kind.OrElse: "??",
    Kind.Bang: "!",
    Kind.Amp: "&",
    Kind.Pipe: "|",
    Kind.Xor: "^",
    Kind.BitNot: "~",
    Kind.Hash: "#",
    Kind.Dollar: "$",
    Kind.Lbrace: "{",
    Kind.Rbrace: "}",
    Kind.Lbracket: "[",
    Kind.Rbracket: "]",
    Kind.Lparen: "(",
    Kind.Rparen: ")",

    # ========== keywords ==========
    Kind.KwAnd: "and",
    Kind.KwAs: "as",
    Kind.KwBase: "base",
    Kind.KwBreak: "break",
    Kind.KwCatch: "catch",
    Kind.KwClass: "class",
    Kind.KwConst: "const",
    Kind.KwContinue: "continue",
    Kind.KwDefer: "defer",
    Kind.KwElse: "else",
    Kind.KwEnum: "enum",
    Kind.KwErrDefer: "errdefer",
    Kind.KwExtend: "extend",
    Kind.KwExtern: "extern",
    Kind.KwFalse: "false",
    Kind.KwFor: "for",
    Kind.KwFn: "fn",
    Kind.KwIf: "if",
    Kind.KwImport: "import",
    Kind.KwIn: "in",
    Kind.KwIs: "is",
    Kind.KwLet: "let",
    Kind.KwMut: "mut",
    Kind.KwNil: "nil",
    Kind.KwNotIn: "!in",
    Kind.KwNotIs: "!is",
    Kind.KwOr: "or",
    Kind.KwPub: "pub",
    Kind.KwProt: "prot",
    Kind.KwReturn: "return",
    Kind.KwSelf: "self",
    Kind.KwSelfTy: "Self",
    Kind.KwStruct: "struct",
    Kind.KwSwitch: "switch",
    Kind.KwTest: "test",
    Kind.KwTrait: "trait",
    Kind.KwTrue: "true",
    Kind.KwType: "type",
    Kind.KwUnsafe: "unsafe",
    Kind.KwWhile: "while",
    # ==============================
}

OVERLOADABLE_OPERATORS = (
    Kind.Plus, Kind.Minus, Kind.Mul, Kind.Div, Kind.Mod, Kind.Eq, Kind.Ne,
    Kind.Lt, Kind.Gt, Kind.Le, Kind.Ge
)

def generate_overloadable_op_map():
    map = {}
    for op in OVERLOADABLE_OPERATORS:
        if op == Kind.Plus: gname = "_add_"
        elif op == Kind.Minus: gname = "_sub_"
        elif op == Kind.Mul: gname = "_mul_"
        elif op == Kind.Div: gname = "_div_"
        elif op == Kind.Mod: gname = "_mod_"
        elif op == Kind.Eq: gname = "_eq_"
        elif op == Kind.Ne: gname = "_ne_"
        elif op == Kind.Lt: gname = "_lt_"
        elif op == Kind.Gt: gname = "_gt_"
        elif op == Kind.Le: gname = "_le_"
        elif op == Kind.Ge: gname = "_ge_"
        else: assert False
        map[str(op)] = gname
    return map

OVERLOADABLE_OPERATORS_STR = generate_overloadable_op_map()

def generate_keyword_map():
    res = {}
    for i, k in enumerate(Kind):
        if i > Kind.KeywordBegin - 1 and i < Kind.KeywordEnd - 1:
            res[str(k)] = k
    return res

KEYWORDS = generate_keyword_map()

def lookup(lit):
    return KEYWORDS[lit] if lit in KEYWORDS else Kind.Name

def is_key(lit):
    return lit in KEYWORDS

class Pos:
    def __init__(self, file, line, col, pos):
        self.file = file
        self.line = line
        self.col = col
        self.pos = pos

    def __repr__(self):
        return f"{self.file}:{self.line+1}:{self.col}"

    def __str__(self):
        return self.__repr__()

NO_POS = Pos("", 0, 0, 0)

class Token:
    def __init__(self, lit, kind, pos):
        self.lit = lit
        self.kind = kind
        self.pos = pos

    def __str__(self):
        string = str(self.kind)
        if not string[0].isalpha():
            return f"token `{string}`"
        if is_key(self.lit):
            string = "keyword"
        if self.lit != "" and self.kind != Kind.DocComment:
            string += f" `{self.lit}`"
        return string

    def __repr__(self):
        return f'rivet.Token(kind: "{self.kind}", lit: "{self.lit}", pos: "{self.pos}")'
