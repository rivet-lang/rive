# Copyright (C) 2022 The Rivet Team. All rights reserved.
# Use of this source code is governed by an MIT license
# that can be found in the LICENSE file.

from enum import IntEnum as Enum, auto as auto_enum

from . import type

class SourceFile:
    def __init__(self, file, decls):
        self.file = file
        self.decls = decls

class Visibility(Enum):
    Private = auto_enum()
    Public = auto_enum() # Public outside current module
    PublicInPkg = auto_enum() # Public inside current package

    def is_pub(self):
        return self != Visibility.Private

    def __repr__(self):
        if self == Visibility.Public:
            return "pub"
        elif self == Visibility.PublicInPkg:
            return "pub(pkg)"
        return "" # private

    def __str__(self):
        return self.__repr__()

# ---- Declarations ----
class EmptyDecl:
    pass

class DocComment:
    def __init__(self, lines, pos):
        self.lines = lines
        self.pos = pos

    def has_doc(self):
        return len(self.lines) > 0

    def merge(self):
        res = ""
        for l in self.lines:
            res += l
            if len(l) == 0 or l.endswith("."):
                res += "\n"
            else:
                res += " "
        return res

class Attr:
    def __init__(self, name, pos, expr=None, has_expr=False):
        self.name = name
        self.pos = pos
        self.expr = expr
        self.has_expr = has_expr

class Attrs:
    def __init__(self):
        self.attrs = []

    def add(self, attr):
        self.attrs.append(attr)

    def lookup(self, name):
        for attr in self.attrs:
            if attr.name == name:
                return attr
        return None

    def has_attrs(self):
        return len(self.attrs) > 0

class ExternPkg:
    def __init__(self, pkg_name, pos):
        self.pkg_name = pkg_name
        self.pos = pos

class ExternDecl:
    def __init__(self, abi, protos, pos):
        self.abi = abi
        self.protos = protos
        self.pos = pos

class ConstDecl:
    def __init__(self, vis, name, typ, expr):
        self.vis = vis
        self.name = name
        self.typ = typ
        self.expr = expr

class StaticDecl:
    def __init__(self, vis, is_mut, name, typ, expr):
        self.vis = vis
        self.is_mut = is_mut
        self.name = name
        self.typ = typ
        self.expr = expr

class ModDecl:
    def __init__(self, doc_comment, attrs, name, vis, decls, pos):
        self.doc_comment = doc_comment
        self.attrs = attrs
        self.name = name
        self.vis = vis
        self.decls = decls
        self.pos = pos

class TypeDecl:
    def __init__(self, vis, name, parent, pos):
        self.vis = vis
        self.name = name
        self.parent = parent
        self.pos = pos

class ErrTypeDecl:
    def __init__(self, vis, name, pos):
        self.vis = vis
        self.name = name
        self.pos = pos

class TraitDecl:
    def __init__(self, vis, name, decls, pos):
        self.vis = vis
        self.name = name
        self.decls = decls
        self.pos = pos

class UnionDecl:
    def __init__(self, vis, name, variants, decls, pos):
        self.vis = vis
        self.name = name
        self.variants = variants
        self.decls = decls
        self.pos = pos

class StructField:
    def __init__(self, is_pub, is_mut, name, typ, def_expr, has_def_expr):
        self.is_pub = is_pub
        self.is_mut = is_mut
        self.name = name
        self.typ = typ
        self.def_expr = def_expr
        self.has_def_expr = has_def_expr

class StructDecl:
    def __init__(self, vis, name, decls, pos):
        self.vis = vis
        self.name = name
        self.decls = decls
        self.pos = pos

class EnumDecl:
    def __init__(self, vis, name, variants, decls, pos):
        self.vis = vis
        self.name = name
        self.variants = variants
        self.decls = decls
        self.pos = pos

class ExtendDecl:
    def __init__(self, typ, decls):
        self.typ = typ
        self.decls = decls

class FnDecl:
    def __init__(
        self,
        doc_comment,
        attrs,
        vis,
        is_unsafe,
        name,
        args,
        ret_is_mut,
        ret_typ,
        stmts,
        has_body=False,
        is_method=False,
        self_is_ref=False,
        self_is_mut=False
    ):
        self.doc_comment = doc_comment
        self.attrs = attrs
        self.vis = vis
        self.is_unsafe = is_unsafe
        self.is_method = is_method
        self.self_is_ref = self_is_ref
        self.self_is_mut = self_is_mut
        self.name = name
        self.args = args
        self.ret_is_mut = ret_is_mut
        self.ret_typ = ret_typ
        self.stmts = stmts

class TestDecl:
    def __init__(self, name, stmts):
        self.name = name
        self.stmts = stmts

class DestructorDecl:
    def __init__(self, stmts):
        self.stmts = stmts

# ------ Statements --------
class VarDecl:
    def __init__(self, is_mut, is_ref, name, typ):
        self.is_mut = is_mut
        self.is_ref = is_ref
        self.name = name
        self.typ = typ

class LetStmt:
    def __init__(self, lefts, right, pos):
        self.lefts = lefts
        self.right = right
        self.pos = pos

class AssignStmt:
    def __init__(self, left, op, right, pos):
        self.left = left
        self.op = op
        self.right = right
        self.pos = pos

class LabelStmt:
    def __init__(self, label, pos):
        self.label = label
        self.pos = pos

class LoopStmt:
    def __init__(self, stmt):
        self.stmt = stmt

class WhileStmt:
    def __init__(self, cond, stmt):
        self.cond = cond
        self.stmt = stmt

class ForInStmt:
    def __init__(self, lefts, iterable, stmt):
        self.lefts = lefts
        self.iterable = iterable
        self.stmt = stmt

class GotoStmt:
    def __init__(self, label, pos):
        self.label = label
        self.pos = pos

class BranchStmt:
    def __init__(self, op, pos):
        self.op = op
        self.pos = pos

class ReturnStmt:
    def __init__(self, expr, has_expr, pos):
        self.expr = expr
        self.has_expr = has_expr
        self.pos = pos

class RaiseStmt:
    def __init__(self, msg, pos):
        self.msg = msg
        self.pos = pos

class ExprStmt:
    def __init__(self, expr, pos):
        self.expr = expr
        self.pos = pos

    def __repr__(self):
        return f"{self.expr}"

    def __str__(self):
        return self.__repr__()

# ------ Expressions -------
class EmptyExpr:
    def __init__(self, pos):
        self.pos = pos

    def __repr__(self):
        return f'rivet.EmptyExpr(pos: "{self.pos}")'

    def __str__(self):
        return self.__repr__()

class Block:
    def __init__(self, is_unsafe, stmts, expr, is_expr, pos):
        self.is_unsafe = is_unsafe
        self.stmts = stmts
        self.expr = expr
        self.is_expr = is_expr
        self.typ = None
        self.pos = pos

    def __repr__(self):
        prefix = "unsafe " if self.is_unsafe else ""
        if len(self.stmts) == 0:
            if self.is_expr:
                return f"{prefix}{{ {self.expr} }}"
            else:
                return f"{prefix}{{}}"
        if self.is_expr:
            return f"{prefix}{{ {'; '.join([str(s) for s in self.stmts])}; {self.expr} }}"
        if len(self.stmts) == 1:
            return f"{prefix}{{ {self.stmts[0]}; }}"
        return f"{prefix}{{ {'; '.join([str(s) for s in self.stmts])}; }}"

    def __str__(self):
        return self.__repr__()

class TypeNode:
    def __init__(self, typ, pos):
        self.typ = typ
        self.pos = pos

    def __repr__(self):
        return f"{self.typ}"

    def __str__(self):
        return self.__repr__()

class PkgExpr:
    def __init__(self, pos):
        self.pos = pos

    def __repr__(self):
        return "pkg"

    def __str__(self):
        return self.__repr__()

class Ident:
    def __init__(self, name, pos, scope, is_comptime):
        self.name = name
        self.scope = scope
        self.is_comptime = is_comptime
        self.ty = None
        self.pos = pos

    def __repr__(self):
        if self.is_comptime:
            return f"${self.name}"
        return self.name

    def __str__(self):
        return self.__repr__()

class EnumVariantExpr:
    def __init__(self, variant, pos):
        self.variant = variant
        self.pos = pos
        self.ty = None

    def __repr__(self):
        return f".{self.variant}"

    def __str__(self):
        return self.__repr__()

class NoneLiteral:
    def __init__(self, pos):
        self.pos = pos
        self.typ = None

    def __repr__(self):
        return "none"

    def __str__(self):
        return self.__repr__()

class BoolLiteral:
    def __init__(self, lit, pos):
        self.lit = lit
        self.pos = pos
        self.typ = None

    def __repr__(self):
        return "true" if self.lit else "false"

    def __str__(self):
        return self.__repr__()

class CharLiteral:
    def __init__(self, lit, pos, is_byte=False):
        self.lit = lit
        self.pos = pos
        self.is_byte = is_byte
        self.typ = None

    def __repr__(self):
        p = "b" if self.is_byte else ""
        return f"{p}'{self.lit}'"

    def __str__(self):
        return self.__repr__()

class IntegerLiteral:
    def __init__(self, lit, pos):
        self.lit = lit
        self.pos = pos

    def __repr__(self):
        return self.lit

    def __str__(self):
        return self.__repr__()

class FloatLiteral:
    def __init__(self, lit, pos):
        self.lit = lit
        self.pos = pos

    def __repr__(self):
        return self.lit

    def __str__(self):
        return self.__repr__()

class StringLiteral:
    def __init__(self, lit, is_raw, is_bytestr, pos):
        self.lit = lit
        self.is_raw = is_raw
        self.is_bytestr = is_bytestr
        self.pos = pos
        self.typ = None

    def __repr__(self):
        p = "b" if self.is_bytestr else "r" if self.is_raw else ""
        return f'{p}"{self.lit}"'

    def __str__(self):
        return self.__repr__()

class SelfExpr:
    def __init__(self, scope, pos):
        self.scope = scope
        self.pos = pos

    def __repr__(self):
        return "self"

    def __str__(self):
        return self.__repr__()

class SelfTyExpr:
    def __init__(self, scope, pos):
        self.scope = scope
        self.pos = pos

    def __repr__(self):
        return "Self"

    def __str__(self):
        return self.__repr__()

class TupleLiteral:
    def __init__(self, exprs, pos):
        self.exprs = exprs
        self.typ = None
        self.pos = pos

    def __repr__(self):
        return f"({', '.join([str(e) for e in self.exprs])})"

    def __str__(self):
        return self.__repr__()

class StructLiteral:
    def __init__(self, expr, fields, pos):
        self.expr = expr
        self.fields = fields
        self.typ = None
        self.field_types = {}
        self.pos = pos

class ArrayLiteral:
    def __init__(self, elems, pos):
        self.elems = elems
        self.typ = None
        self.pos = pos

    def __repr__(self):
        if len(self.elems) == 0:
            return "[]"
        return f"[{', '.join([str(e) for e in self.elems])}]"

    def __str__(self):
        return self.__repr__()

class GoExpr:
    def __init__(self, expr, pos):
        self.expr = expr
        self.pos = pos
        self.typ = None

    def __repr__(self):
        return f"go {self.expr}"

    def __str__(self):
        return self.__repr__()

class TryExpr:
    def __init__(self, expr, pos):
        self.expr = expr
        self.pos = pos

    def __repr__(self):
        return f"try {self.expr}"

    def __str__(self):
        return self.__repr__()

class CastExpr:
    def __init__(self, expr, typ, pos):
        self.expr = expr
        self.typ = typ
        self.pos = pos

    def __repr__(self):
        return f"cast({self.expr}, {self.typ})"

    def __str__(self):
        return self.__repr__()

class NoneCheckExpr:
    def __init__(self, expr, pos):
        self.expr = expr
        self.typ = None
        self.pos = pos

    def __repr__(self):
        return f"{self.expr}.?"

    def __str__(self):
        return self.__repr__()

class IndirectExpr:
    def __init__(self, expr, pos):
        self.expr = expr
        self.typ = None
        self.pos = pos

    def __repr__(self):
        return f"{self.expr}.*"

    def __str__(self):
        return self.__repr__()

class OrElseExpr:
    def __init__(self, left, right, pos):
        self.left = left
        self.right = right
        self.pos = pos

    def __repr__(self):
        return f"{self.left} orelse {self.right}"

    def __str__(self):
        return self.__repr__()

class GuardExpr:
    # if (let x = optional_or_result_fn()) { ... }
    # if (let x = "".split(", "); x.len > 5) { ... }
    def __init__(self, ident, is_mut, expr, pos):
        self.ident = ident
        self.is_mut = is_mut
        self.expr = expr
        self.pos = pos

    def __repr__(self):
        kmut = "mut" if self.is_mut else ""
        return f"let {kmut} {self.ident} = {self.expr}"

    def __str__(self):
        return self.__repr__()

class IfBranch:
    def __init__(self, is_comptime, cond, expr, is_else, op):
        self.is_comptime = is_comptime
        self.cond = cond
        self.expr = expr
        self.is_else = is_else
        self.op = op

    def __repr__(self):
        prefix = "$" if self.is_comptime else ""
        if self.is_else:
            return f"{prefix}else {self.expr}"
        return f"{prefix}{self.op} ({self.cond}) {self.expr}"

    def __str__(self):
        return self.__repr__()

class IfExpr:
    def __init__(self, is_comptime, branches, pos):
        self.is_comptime = is_comptime
        self.branches = branches
        self.pos = pos
        self.typ = None

    def __repr__(self):
        return " ".join([str(b) for b in self.branches])

    def __str__(self):
        return self.__repr__()

class MatchBranch:
    def __init__(self, pats, expr, is_else):
        self.pats = pats
        self.expr = expr
        self.is_else = is_else

    def __repr__(self):
        if self.is_else:
            return f"else => {self.expr}"
        return f"{', '.join([str(p) for p in self.pats])} => {self.expr}"

    def __str__(self):
        return self.__repr__()

class MatchExpr:
    def __init__(self, is_comptime, expr, branches, is_typematch, pos):
        self.expr = expr
        self.branches = branches
        self.is_typematch = is_typematch
        self.is_comptime = is_comptime
        self.pos = pos
        self.typ = None

    def __repr__(self):
        prefix = "$" if self.is_comptime else ""
        kis = " is " if self.is_typematch else " "
        return f"{prefix}match ({self.expr}){kis}{{ " + ", ".join(
            [str(b) for b in self.branches]
        ) + " }"

    def __str__(self):
        return self.__repr__()

class UnaryExpr:
    def __init__(self, right, op, pos=None):
        self.right = right
        self.op = op
        self.pos = pos
        self.typ = None

    def __repr__(self):
        return f"{self.op}{self.right}"

    def __str__(self):
        return self.__repr__()

class BinaryExpr:
    def __init__(self, left, op, right, pos=None):
        self.left = left
        self.op = op
        self.right = right
        self.pos = pos
        self.typ = None

    def __repr__(self):
        return f"{self.left} {self.op} {self.right}"

    def __str__(self):
        return self.__repr__()

class PostfixExpr:
    def __init__(self, left, op, pos=None):
        self.left = left
        self.op = op
        self.pos = pos
        self.typ = None

    def __repr__(self):
        return f"{self.left}{self.op}"

    def __str__(self):
        return self.__repr__()

class ParExpr:
    def __init__(self, expr, pos):
        self.expr = expr
        self.typ = None
        self.pos = pos

    def __repr__(self):
        return f"({self.expr})"

class IndexExpr:
    def __init__(self, left, index, pos):
        self.left = left
        self.index = index
        self.left_typ = None
        self.typ = None
        self.pos = pos

    def __repr__(self):
        return f"{self.left}[{self.index}]"

    def __str__(self):
        return self.__repr__()

class CallExpr:
    def __init__(self, left, args, err_handler, pos):
        self.left = left
        self.args = args
        self.err_handler = err_handler
        self.info = None
        self.typ = None
        self.pos = pos

    def get_named_arg(self, name):
        for arg in self.args:
            if arg.is_named and arg.name == name:
                return arg
        return None

    # Returns the number of pure arguments, that is, not named, that
    # this call has.
    def pure_args_count(self):
        l = 0
        for arg in self.args:
            if not arg.is_named:
                l += 1
        return l

    def has_err_handler(self):
        return self.err_handler.expr != None

    def __repr__(self):
        res = f"{self.left}({', '.join([str(a) for a in self.args])})"
        if self.has_err_handler():
            res += " " + str(self.err_handler)
        return res

    def __str__(self):
        return self.__repr__()

class CallArg:
    def __init__(self, expr, pos, name=""):
        self.expr = expr
        self.pos = pos
        self.name = name
        self.is_named = name != ""

    def __repr__(self):
        if self.is_named:
            return f"{self.name}: {self.expr}"
        return str(self.expr)

    def __str__(self):
        return self.__repr__()

class CallErrorHandler:
    def __init__(self, varname, expr):
        self.varname = varname
        self.expr = expr

    def __repr__(self):
        if len(self.varname) == 0:
            return f"catch {self.expr}"
        return f"catch |{self.varname}| {self.expr}"

    def __str__(self):
        return self.__repr__()

class RangeExpr:
    def __init__(
        self, start, end, is_inclusive, pos, has_start=True, has_end=True
    ):
        self.start = start
        self.end = end
        self.is_inclusive = is_inclusive
        self.has_start = has_start
        self.has_end = has_end
        self.pos = pos
        self.typ = None

    def __repr__(self):
        sep = "=" if self.is_inclusive else ""
        return f"{self.start}..{sep}{self.end}"

    def __str__(self):
        return self.__repr__()

class BuiltinCallExpr:
    def __init__(self, left, args, pos):
        self.left = left
        self.args = args
        self.typ = None
        self.pos = pos

    def __repr__(self):
        return f"{self.left}!({', '.join([str(a) for a in self.args])})"

    def __str__(self):
        return self.__repr__()

class SelectorExpr:
    def __init__(self, left, field_name, pos):
        self.left = left
        self.field_name = field_name
        self.field_info = None
        self.left_typ = None
        self.typ = None
        self.pos = pos

    def __repr__(self):
        return f"{self.left}.{self.field_name}"

    def __str__(self):
        return self.__repr__()

class PathExpr:
    def __init__(self, left, field_name, pos):
        self.left = left
        self.field_name = field_name
        self.field_info = None
        self.left_typ = None
        self.typ = None
        self.is_last = False
        self.pos = pos

    def __repr__(self):
        return f"{self.left}::{self.field_name}"

    def __str__(self):
        return self.__repr__()
