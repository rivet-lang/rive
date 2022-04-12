# Copyright (C) 2022 The Rivet Team. All rights reserved.
# Use of this source code is governed by an MIT license
# that can be found in the LICENSE file.

from enum import IntEnum as Enum, auto as auto_enum

# from . import utils


class Kind:
    Unknown = auto_enum()  # unknown
    EOF = auto_enum()  # end of file
    Name = auto_enum()  # name
    Number = auto_enum()  # number
    Char = auto_enum()  # character
    String = auto_enum()  # string
    Plus = auto_enum()  # +
    Minus = auto_enum()  # -
    Mult = auto_enum()  # *
    Div = auto_enum()  # /
    Mod = auto_enum()  # %
    Inc = auto_enum()  # ++
    Dec = auto_enum()  # --
    Assign = auto_enum()  # =
    PlusAssign = auto_enum()  # +=
    MinusAssign = auto_enum()  # -=
    MultAssign = auto_enum()  # *=
    DivAssign = auto_enum()  # /=
    ModAssign = auto_enum()  # %=
    AmpAssign = auto_enum()  # &=
    PipeAssign = auto_enum()  # |=
    XorAssign = auto_enum()  # ^=
    Eq = auto_enum()  # ==
    Ne = auto_enum()  # !=
    Lt = auto_enum()  # <
    Gt = auto_enum()  # >
    Le = auto_enum()  # <=
    Ge = auto_enum()  # >=
    Lshift = auto_enum()  # <<
    Rshift = auto_enum()  # >>
    Dot = auto_enum()  # .
    DotDot = auto_enum()  # ..
    Ellipsis = auto_enum()  # ...
    Arrow = auto_enum()  # =>
    Comma = auto_enum()  # ,
    Colon = auto_enum()  # :
    DoubleColon = auto_enum()  # ::
    Semicolon = auto_enum()  # ;
    Question = auto_enum()  # ?
    Bang = auto_enum()  # !
    Amp = auto_enum()  # &
    Pipe = auto_enum()  # |
    BitNot = auto_enum()  # ~
    Xor = auto_enum()  # ^
    At = auto_enum()  # @
    Hash = auto_enum()  # #
    Dollar = auto_enum()  # $
    Lbrace = auto_enum()  # {
    Rbrace = auto_enum()  # }
    Lbracket = auto_enum()  # [
    Rbracket = auto_enum()  # ]
    Lparen = auto_enum()  # (
    Rparen = auto_enum()  # )

    KeywordBegin = auto_enum()
    # ========== literals ==========
    KeyNone = auto_enum()  # none
    KeyTrue = auto_enum()  # true
    KeyFalse = auto_enum()  # false
    KeyBase = auto_enum()  # base
    KeySelf = auto_enum()  # self
    # ==============================

    # ========== keywords ==========
    KeySelfTy = auto_enum()  # Self
    KeyPkg = auto_enum()  # pkg
    KeyMod = auto_enum()  # mod
    KeyExtern = auto_enum()  # extern
    KeyTrait = auto_enum()  # trait
    KeyStruct = auto_enum()  # struct
    KeyUnion = auto_enum()  # union
    KeyType = auto_enum()  # type
    KeyEnum = auto_enum()  # enum
    KeyExtend = auto_enum()  # extend
    KeyFn = auto_enum()  # fn
    KeyTest = auto_enum()  # test
    KeyPub = auto_enum()  # pub
    KeyIf = auto_enum()  # if
    KeyElif = auto_enum()  # elif
    KeyElse = auto_enum()  # else
    KeyMatch = auto_enum()  # match
    KeyLoop = auto_enum()  # loop
    KeyWhile = auto_enum()  # while
    KeyFor = auto_enum()  # for
    KeyBreak = auto_enum()  # break
    KeyContinue = auto_enum()  # continue
    KeyReturn = auto_enum()  # return
    KeyRaise = auto_enum()  # raise
    KeyGoto = auto_enum()  # goto
    KeyAnd = auto_enum()  # and
    KeyOr = auto_enum()  # or
    KeyIn = auto_enum()  # in
    KeyIs = auto_enum()  # is
    KeyNotIn = auto_enum()  # !in
    KeyNotIs = auto_enum()  # !is
    KeyCast = auto_enum()  # cast
    KeyAs = auto_enum()  # as
    KeyUse = auto_enum()  # use
    KeyConst = auto_enum()  # const
    KeyStatic = auto_enum()  # static
    KeyLet = auto_enum()  # let
    KeyMut = auto_enum()  # mut
    KeyDefer = auto_enum()  # defer
    KeyUnsafe = auto_enum()  # unsafe
    KeyOrElse = auto_enum()  # orelse
    KeyCatch = auto_enum()  # catch
    # ==============================

    KeywordEnd = auto_enum()
