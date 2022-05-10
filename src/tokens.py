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
    Mult = auto_enum() # *
    Div = auto_enum() # /
    Mod = auto_enum() # %
    Inc = auto_enum() # ++
    Dec = auto_enum() # --
    Assign = auto_enum() # =
    PlusAssign = auto_enum() # +=
    MinusAssign = auto_enum() # -=
    MultAssign = auto_enum() # *=
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
    Bang = auto_enum() # !
    Amp = auto_enum() # &
    Pipe = auto_enum() # |
    BitNot = auto_enum() # ~
    Xor = auto_enum() # ^
    Hash = auto_enum() # #
    Dollar = auto_enum() # $
    Lbrace = auto_enum() # {
    Rbrace = auto_enum() # }
    Lbracket = auto_enum() # [
    Rbracket = auto_enum() # ]
    Lparen = auto_enum() # (
    Rparen = auto_enum() # )

    KeywordBegin = auto_enum()
    # ========== literals ==========
    KeyNone = auto_enum() # none
    KeyTrue = auto_enum() # true
    KeyFalse = auto_enum() # false
    KeyBase = auto_enum() # base
    KeySelf = auto_enum() # self
    # ==============================

    # ========== KEYWORDS ==========
    KeySelfTy = auto_enum() # Self
    KeyPkg = auto_enum() # pkg
    KeyPub = auto_enum() # pub
    KeyUse = auto_enum() # use
    KeyAs = auto_enum() # as
    KeyConst = auto_enum() # const
    KeyStatic = auto_enum() # static
    KeyMod = auto_enum() # mod
    KeyExtern = auto_enum() # extern
    KeyTrait = auto_enum() # trait
    KeyUnion = auto_enum() # union
    KeyStruct = auto_enum() # struct
    KeyEnum = auto_enum() # enum
    KeyErrType = auto_enum() # errtype
    KeyType = auto_enum() # type
    KeyExtend = auto_enum() # extend
    KeyTest = auto_enum() # test
    KeyFn = auto_enum() # fn
    KeyLet = auto_enum() # let
    KeyMut = auto_enum() # mut
    KeyIf = auto_enum() # if
    KeyElif = auto_enum() # elif
    KeyElse = auto_enum() # else
    KeyMatch = auto_enum() # match
    KeyLoop = auto_enum() # loop
    KeyWhile = auto_enum() # while
    KeyFor = auto_enum() # for
    KeyContinue = auto_enum() # continue
    KeyBreak = auto_enum() # break
    KeyReturn = auto_enum() # return
    KeyRaise = auto_enum() # raise
    KeyGoto = auto_enum() # goto
    KeyGo = auto_enum() # go
    KeyAnd = auto_enum() # and
    KeyOr = auto_enum() # or
    KeyIn = auto_enum() # in
    KeyIs = auto_enum() # is
    KeyNotIn = auto_enum() # !in
    KeyNotIs = auto_enum() # !is
    KeyCast = auto_enum() # cast
    KeyUnsafe = auto_enum() # unsafe
    KeyTry = auto_enum() # try
    KeyOrElse = auto_enum() # orelse
    KeyCatch = auto_enum() # catch
    # ==============================

    KeywordEnd = auto_enum()

    def is_assign(self):
        return self in [
            Kind.Assign,
            Kind.PlusAssign,
            Kind.MinusAssign,
            Kind.MultAssign,
            Kind.DivAssign,
            Kind.ModAssign,
            Kind.AmpAssign,
            Kind.PipeAssign,
            Kind.XorAssign,
        ]

    def is_relational(self):
        return self in [
            Kind.Eq,
            Kind.Ne,
            Kind.Lt,
            Kind.Gt,
            Kind.Le,
            Kind.Ge,
            Kind.KeyIs,
            Kind.KeyNotIs,
        ]

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
    Kind.Mult: "*",
    Kind.Div: "/",
    Kind.Mod: "%",
    Kind.Inc: "++",
    Kind.Dec: "--",
    Kind.Assign: "=",
    Kind.PlusAssign: "+=",
    Kind.MinusAssign: "-=",
    Kind.MultAssign: "*=",
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
    Kind.Bang: "!",
    Kind.Amp: "&",
    Kind.Pipe: "|",
    Kind.BitNot: "~",
    Kind.Xor: "^",
    Kind.Hash: "#",
    Kind.Dollar: "$",
    Kind.Lbrace: "{",
    Kind.Rbrace: "}",
    Kind.Lbracket: "[",
    Kind.Rbracket: "]",
    Kind.Lparen: "(",
    Kind.Rparen: ")",

    # ========== literals ==========
    Kind.KeyNone: "none",
    Kind.KeyTrue: "true",
    Kind.KeyFalse: "false",
    Kind.KeyBase: "base",
    Kind.KeySelf: "self",
    # ==============================

    # ========== KEYWORDS ==========
    Kind.KeySelfTy: "Self",
    Kind.KeyPkg: "pkg",
    Kind.KeyPub: "pub",
    Kind.KeyUse: "use",
    Kind.KeyAs: "as",
    Kind.KeyConst: "const",
    Kind.KeyStatic: "static",
    Kind.KeyMod: "mod",
    Kind.KeyExtern: "extern",
    Kind.KeyTrait: "trait",
    Kind.KeyUnion: "union",
    Kind.KeyStruct: "struct",
    Kind.KeyEnum: "enum",
    Kind.KeyErrType: "errtype",
    Kind.KeyType: "type",
    Kind.KeyExtend: "extend",
    Kind.KeyTest: "test",
    Kind.KeyFn: "fn",
    Kind.KeyLet: "let",
    Kind.KeyMut: "mut",
    Kind.KeyIf: "if",
    Kind.KeyElif: "elif",
    Kind.KeyElse: "else",
    Kind.KeyMatch: "match",
    Kind.KeyLoop: "loop",
    Kind.KeyWhile: "while",
    Kind.KeyFor: "for",
    Kind.KeyContinue: "continue",
    Kind.KeyBreak: "break",
    Kind.KeyReturn: "return",
    Kind.KeyRaise: "raise",
    Kind.KeyGoto: "goto",
    Kind.KeyGo: "go",
    Kind.KeyAnd: "and",
    Kind.KeyOr: "or",
    Kind.KeyIn: "in",
    Kind.KeyIs: "is",
    Kind.KeyNotIn: "!in",
    Kind.KeyNotIs: "!is",
    Kind.KeyCast: "cast",
    Kind.KeyUnsafe: "unsafe",
    Kind.KeyTry: "try",
    Kind.KeyOrElse: "orelse",
    Kind.KeyCatch: "catch",
    # ==============================
}

def generate_keyword_map():
    res = {}
    for (i, k) in enumerate(Kind):
        if i > Kind.KeywordBegin - 1 and i < Kind.KeywordEnd - 1:
            res[str(k)] = k
    return res

KEYWORDS = generate_keyword_map()

def lookup(lit):
    return KEYWORDS[lit] if lit in KEYWORDS else Kind.Name

def is_key(lit):
    return lookup(lit) != Kind.Name

class Position:
    def __init__(self, file, line, col, pos):
        self.file = file
        self.line = line
        self.col = col
        self.pos = pos

    def __repr__(self):
        return f"{self.file}:{self.line+1}:{self.col}"

    def __str__(self):
        return self.__repr__()

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
