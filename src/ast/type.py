# Copyright (C) 2022 The Rivet Team. All rights reserved.
# Use of this source code is governed by an MIT license
# that can be found in the LICENSE file.

from . import Visibility
from ..tokens import Position
from .sym import TypeKind, Fn as FnInfo, Arg

class _Ptr: # ugly hack =/
    def __init__(self, val):
        self.val = val

    def store(self, val):
        self.val.__class__ = val.__class__
        self.val.__dict__ = val.__dict__

class BaseType:
    def get_sym(self):
        if isinstance(self, Type) or isinstance(self, Slice) or isinstance(
            self, Array
        ) or isinstance(self, Tuple):
            return self.sym
        elif isinstance(self, Fn):
            return None
        return self.typ.get_sym()

    def unalias(self):
        if isinstance(self, Result):
            self.typ.unalias()
        elif isinstance(self, Optional):
            self.typ.unalias()
        elif isinstance(self, Fn):
            for arg in self.args:
                arg.typ.unalias()
            self.ret_typ.unalias()
        elif isinstance(self, Tuple):
            for t in self.types:
                t.unalias()
        elif isinstance(self, Array):
            self.typ.unalias()
        elif isinstance(self, Slice):
            self.typ.unalias()
        elif isinstance(self, Ptr):
            self.typ.unalias()
        elif isinstance(self, Ref):
            self.typ.unalias()
        elif isinstance(self, Type):
            if self.is_resolved() and self.sym.kind == TypeKind.Alias:
                self.sym.info.parent.unalias()
                _Ptr(self).store(self.sym.info.parent)

class Type(BaseType):
    def __init__(self, sym):
        self.sym = sym
        self.expr = None
        self.unresolved = False

    @staticmethod
    def unresolved(expr):
        typ = Type(None)
        typ.expr = expr
        typ.unresolved = True
        return typ

    def resolve(self, sym):
        self.sym = sym
        self.unresolved = False

    def is_resolved(self):
        return not self.unresolved

    def qualstr(self):
        return self.sym.qualname()

    def __str__(self):
        if self.unresolved:
            return str(self.expr)
        return str(self.sym.name)

class Ref(BaseType):
    def __init__(self, typ):
        self.typ = typ

    def qualstr(self):
        return f"&{self.typ.qualstr()}"

    def __str__(self):
        return f"&{self.typ}"

class Ptr(BaseType):
    def __init__(self, typ):
        self.typ = typ

    def qualstr(self):
        return f"*{self.typ.qualstr()}"

    def __str__(self):
        return f"*{self.typ}"

class Slice(BaseType):
    def __init__(self, typ):
        self.typ = typ
        self.sym = None

    def resolve(self, sym):
        self.sym = sym

    def qualstr(self):
        return f"[{self.typ.qualstr()}]"

    def __str__(self):
        return f"[{self.typ}]"

class Array(BaseType):
    def __init__(self, typ, size):
        self.typ = typ
        self.size = size
        self.sym = None

    def resolve(self, sym):
        self.sym = sym

    def qualstr(self):
        return f"[{self.typ.qualstr()}; {self.size}]"

    def __str__(self):
        return f"[{self.typ}; {self.size}]"

class Tuple(BaseType):
    def __init__(self, types):
        self.types = types
        self.sym = None

    def resolve(self, sym):
        self.sym = sym

    def qualstr(self):
        return f"({', '.join([t.qualstr() for t in self.types])})"

    def __str__(self):
        return f"({', '.join([str(t) for t in self.types])})"

class FnArg:
    def __init__(self, is_mut, typ):
        self.is_mut = is_mut
        self.typ = typ

class Fn(BaseType):
    def __init__(self, is_unsafe, is_extern, abi, args, ret_is_mut, ret_typ):
        self.is_unsafe = is_unsafe
        self.is_extern = is_extern
        self.abi = abi
        self.args = args
        self.ret_is_mut = ret_is_mut
        self.ret_typ = ret_typ

    def info(self):
        args = []
        for i, arg in enumerate(self.args):
            args.append(
                Arg(
                    f"arg{i+1}", arg.is_mut, arg.typ, None, False,
                    Position("", 0, 0, 0)
                )
            )
        return FnInfo(
            self.abi, Visibility.Public, self.is_extern, self.is_unsafe, False,
            self.stringify(False), args, self.ret_is_mut, self.ret_typ, False
        )

    def stringify(self, qual):
        res = ""
        if self.is_unsafe:
            res += "unsafe "
        if self.is_extern:
            res += f'extern "{self.abi}" '
        res += "fn("
        for i, arg in enumerate(self.args):
            if arg.is_mut:
                res += "mut "
            if qual:
                res += arg.typ.qualstr()
            else:
                res += str(arg.typ)
            if i < len(self.args) - 1:
                res += ", "
        res += f") "
        if self.ret_is_mut:
            res += "mut "
        if qual:
            res += self.ret_typ.qualstr()
        else:
            res += str(self.ret_typ)
        return res

    def __eq__(self, got):
        if self.is_unsafe != got.is_unsafe:
            return False
        elif self.is_extern != got.is_extern:
            return False
        elif self.abi != got.abi:
            return False
        elif len(self.args) != len(got.args):
            return False
        for i, arg in enumerate(self.args):
            if arg.typ != got.args[i].typ:
                return False
        return self.ret_typ == got.ret_typ

    def __str__(self):
        return self.stringify(False)

class Optional(BaseType):
    def __init__(self, typ):
        self.typ = typ

    def qualstr(self):
        return f"?{self.typ.qualstr()}"

    def __str__(self):
        return f"?{self.typ}"

class Result(BaseType):
    def __init__(self, typ):
        self.typ = typ

    def qualstr(self):
        return f"!{self.typ.qualstr()}"

    def __str__(self):
        return f"!{self.typ}"
