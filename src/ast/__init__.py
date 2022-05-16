# Copyright (C) 2022 The Rivet Team. All rights reserved.
# Use of this source code is governed by an MIT license
# that can be found in the LICENSE file.

from enum import IntEnum as Enum, auto as auto_enum

COMPTIME_CONSTANTS = [
    "_OS_", "_ARCH_", "_ENDIAN_", "_BITS_", "_BACKEND_", "_FILE_", "_FUNCTION_",
    "_LINE_", "_COLUMN_"
]

def is_known_comptime_constant(name):
    return name in COMPTIME_CONSTANTS

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

# Used for `let` and `for` stmts, and guard exprs
class VarDecl:
    def __init__(self, is_mut, is_ref, name, has_typ, typ, pos):
        self.is_mut = is_mut
        self.is_ref = is_ref
        self.name = name
        self.has_typ = has_typ
        self.typ = typ
        self.pos = pos

    def __repr__(self):
        res = ""
        if self.is_mut:
            res += "mut "
        if self.is_ref:
            res += "&"
        res += self.name
        if self.has_typ:
            res += f": {self.typ}"
        return res

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
        self.if_check = True

    def add(self, attr):
        self.attrs.append(attr)

    def lookup(self, name):
        for attr in self.attrs:
            if attr.name == name:
                return attr
        return None

    def has(self, name):
        if _ := self.lookup(name):
            return True
        return False

    def has_attrs(self):
        return len(self.attrs) > 0

class ExternPkg:
    def __init__(self, pkg_name, pos):
        self.attrs = Attrs()
        self.pkg_name = pkg_name
        self.pos = pos

class ExternDecl:
    def __init__(self, attrs, abi, protos, pos):
        self.attrs = attrs
        self.abi = abi
        self.protos = protos
        self.pos = pos

class ConstDecl:
    def __init__(self, doc_comment, attrs, vis, name, typ, expr, pos):
        self.doc_comment = doc_comment
        self.attrs = attrs
        self.vis = vis
        self.name = name
        self.typ = typ
        self.expr = expr
        self.pos = pos

class StaticDecl:
    def __init__(self, doc_comment, attrs, vis, is_mut, name, typ, expr, pos):
        self.doc_comment = doc_comment
        self.attrs = attrs
        self.vis = vis
        self.is_mut = is_mut
        self.name = name
        self.typ = typ
        self.expr = expr
        self.pos = pos

class ModDecl:
    def __init__(self, doc_comment, attrs, name, vis, decls, pos):
        self.doc_comment = doc_comment
        self.attrs = attrs
        self.name = name
        self.vis = vis
        self.decls = decls
        self.sym = None
        self.pos = pos

class TypeDecl:
    def __init__(self, doc_comment, attrs, vis, name, parent, pos):
        self.doc_comment = doc_comment
        self.attrs = attrs
        self.vis = vis
        self.name = name
        self.parent = parent
        self.pos = pos

class ErrTypeDecl:
    def __init__(self, doc_comment, attrs, vis, name, pos):
        self.doc_comment = doc_comment
        self.attrs = attrs
        self.vis = vis
        self.name = name
        self.pos = pos

class TraitDecl:
    def __init__(self, doc_comment, attrs, vis, name, decls, pos):
        self.doc_comment = doc_comment
        self.attrs = attrs
        self.vis = vis
        self.name = name
        self.decls = decls
        self.pos = pos

class UnionDecl:
    def __init__(self, doc_comment, attrs, vis, name, variants, decls, pos):
        self.doc_comment = doc_comment
        self.attrs = attrs
        self.vis = vis
        self.name = name
        self.variants = variants
        self.decls = decls
        self.sym = None
        self.pos = pos

class StructField:
    def __init__(
        self, attrs, doc_comment, is_pub, is_mut, name, typ, def_expr,
        has_def_expr, pos
    ):
        self.doc_comment = doc_comment
        self.attrs = attrs
        self.is_pub = is_pub
        self.is_mut = is_mut
        self.name = name
        self.typ = typ
        self.def_expr = def_expr
        self.has_def_expr = has_def_expr
        self.pos = pos

class StructDecl:
    def __init__(self, doc_comment, attrs, vis, name, decls, pos):
        self.doc_comment = doc_comment
        self.attrs = attrs
        self.vis = vis
        self.name = name
        self.decls = decls
        self.sym = None
        self.pos = pos

class EnumDecl:
    def __init__(self, doc_comment, attrs, vis, name, variants, decls, pos):
        self.doc_comment = doc_comment
        self.attrs = attrs
        self.vis = vis
        self.name = name
        self.variants = variants
        self.decls = decls
        self.sym = None
        self.pos = pos

class ExtendDecl:
    def __init__(self, attrs, typ, decls):
        self.attrs = attrs
        self.typ = typ
        self.decls = decls

class FnDecl:
    def __init__(
        self,
        doc_comment,
        attrs,
        vis,
        is_extern,
        is_unsafe,
        name,
        name_pos,
        args,
        ret_is_mut,
        ret_typ,
        stmts,
        scope,
        has_body=False,
        is_method=False,
        self_is_ref=False,
        self_is_mut=False,
        has_named_args=False
    ):
        self.doc_comment = doc_comment
        self.attrs = attrs
        self.vis = vis
        self.name = name
        self.name_pos = name_pos
        self.args = args
        self.self_is_ref = self_is_ref
        self.self_is_mut = self_is_mut
        self.self_typ = None
        self.is_extern = is_extern
        self.is_unsafe = is_unsafe
        self.is_method = is_method
        self.ret_is_mut = ret_is_mut
        self.ret_typ = ret_typ
        self.has_named_args = has_named_args
        self.sym = None
        self.scope = scope
        self.stmts = stmts

class TestDecl:
    def __init__(self, scope, name, stmts, pos):
        self.name = name
        self.stmts = stmts
        self.scope = scope
        self.pos = pos

class DestructorDecl:
    def __init__(self, scope, stmts, pos):
        self.stmts = stmts
        self.scope = scope
        self.self_typ = None
        self.pos = pos

# ------ Statements --------
class AssignStmt:
    def __init__(self, left, op, right, pos):
        self.left = left
        self.op = op
        self.right = right
        self.pos = pos

class LetStmt:
    def __init__(self, scope, lefts, right, pos):
        self.lefts = lefts
        self.right = right
        self.scope = scope
        self.pos = pos

class LabelStmt:
    def __init__(self, label, pos):
        self.label = label
        self.pos = pos

class WhileStmt:
    def __init__(self, cond, stmt, is_inf):
        self.cond = cond
        self.stmt = stmt
        self.is_inf = is_inf

class ForInStmt:
    def __init__(self, scope, lefts, iterable, stmt):
        self.lefts = lefts
        self.iterable = iterable
        self.scope = scope
        self.stmt = stmt

class GotoStmt:
    def __init__(self, label, pos):
        self.label = label
        self.pos = pos

class ExprStmt:
    def __init__(self, expr, pos):
        self.expr = expr
        self.pos = pos

    def __repr__(self):
        return str(self.expr)

    def __str__(self):
        return self.__repr__()

# ------ Expressions -------
class EmptyExpr:
    def __init__(self, pos):
        self.pos = pos

    def __repr__(self):
        return f'rivetc.EmptyExpr(pos: "{self.pos}")'

    def __str__(self):
        return self.__repr__()

class TypeNode:
    def __init__(self, typ, pos):
        self.typ = typ
        self.pos = pos

    def __repr__(self):
        return str(self.typ)

    def __str__(self):
        return self.__repr__()

class PkgExpr:
    def __init__(self, pos):
        self.pos = pos

    def __repr__(self):
        return "pkg"

    def __str__(self):
        return self.__repr__()

class VoidLiteral:
    def __init__(self, pos):
        self.pos = pos

    def __repr__(self):
        return "()"

    def __str__(self):
        return self.__repr__()

class Ident:
    def __init__(self, name, pos, scope, is_comptime):
        self.name = name
        self.obj = None
        self.sym = None
        self.is_obj = False
        self.is_comptime = is_comptime
        self.scope = scope
        self.pos = pos
        self.typ = None

    def __repr__(self):
        if self.is_comptime:
            return f"${self.name}"
        return self.name

    def __str__(self):
        return self.__repr__()

class SelfExpr:
    def __init__(self, scope, pos):
        self.scope = scope
        self.pos = pos
        self.typ = None

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

class NoneLiteral:
    def __init__(self, pos):
        self.pos = pos

    def __repr__(self):
        return "none"

    def __str__(self):
        return self.__repr__()

class BoolLiteral:
    def __init__(self, lit, pos):
        self.lit = lit
        self.pos = pos

    def __repr__(self):
        return "true" if self.lit else "false"

    def __str__(self):
        return self.__repr__()

class CharLiteral:
    def __init__(self, lit, pos, is_byte):
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

class EnumVariantExpr:
    def __init__(self, variant, pos):
        self.variant = variant
        self.pos = pos
        self.typ = None

    def __repr__(self):
        return f".{self.variant}"

    def __str__(self):
        return self.__repr__()

class StructLiteralField:
    def __init__(self, name, expr, pos):
        self.name = name
        self.expr = expr
        self.pos = pos

    def __repr__(self):
        return f"{self.name}: {self.expr}"

    def __str__(self):
        return self.__repr__()

class StructLiteral:
    def __init__(self, expr, fields, pos):
        self.expr = expr
        self.fields = fields
        self.pos = pos
        self.typ = None

    def __repr__(self):
        return f"{self.expr}{{ {', '.join([str(f) for f in self.fields])} }}"

    def __str__(self):
        return self.__repr__()

class TupleLiteral:
    def __init__(self, exprs, pos):
        self.exprs = exprs
        self.pos = pos
        self.typ = None

    def __repr__(self):
        return f"({', '.join([str(e) for e in self.exprs])})"

    def __str__(self):
        return self.__repr__()

class ArrayLiteral:
    def __init__(self, elems, pos):
        self.elems = elems
        self.pos = pos
        self.typ = None

    def __repr__(self):
        if len(self.elems) == 0:
            return "[]"
        return f"[{', '.join([str(e) for e in self.elems])}]"

    def __str__(self):
        return self.__repr__()

class CastExpr:
    def __init__(self, expr, typ, pos):
        self.expr = expr
        self.pos = pos
        self.typ = typ

    def __repr__(self):
        return f"cast({self.expr}, {self.typ})"

    def __str__(self):
        return self.__repr__()

class GuardExpr:
    # Examples:
    # if (let x = optional_or_result_fn()) { ... }
    # if (let x = "A,B,C,D".split(","); x.len > 5) { ... }
    # while (let byte = reader.read()) { ... }
    def __init__(self, vars, expr, has_cond, cond, pos):
        self.vars = vars
        self.expr = expr
        self.has_cond = has_cond
        self.cond = cond
        self.pos = pos

    def __repr__(self):
        if len(self.vars) == 1:
            vars_str = str(self.vars[0])
        else:
            vars_str = f"({', '.join([str(v) for v in self.vars])})"
        res = f"let {vars_str} = {self.expr}"
        if self.has_cond:
            res += f"; {self.cond}"
        return res

    def __str__(self):
        return self.__repr__()

class UnaryExpr:
    def __init__(self, right, op, pos):
        self.op = op
        self.right = right
        self.pos = pos
        self.typ = None

    def __repr__(self):
        return f"{self.op}{self.right}"

    def __str__(self):
        return self.__repr__()

class BinaryExpr:
    def __init__(self, left, op, right, pos):
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
    def __init__(self, left, op, pos):
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
        self.pos = pos
        self.typ = None

    def __repr__(self):
        return f"({self.expr})"

class IndexExpr:
    def __init__(self, left, index, pos):
        self.left = left
        self.index = index
        self.left_typ = None
        self.pos = pos
        self.typ = None

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
        self.pos = pos
        self.typ = None

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
        return self.err_handler.expr != None or self.err_handler.is_propagate

    def __repr__(self):
        res = f"{self.left}({', '.join([str(a) for a in self.args])})"
        if self.has_err_handler():
            res += f" {self.err_handler}"
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
    def __init__(self, is_propagate, varname, expr, varname_pos, scope, pos):
        self.is_propagate = is_propagate
        self.varname = varname
        self.varname_pos = varname_pos
        self.expr = expr
        self.scope = scope
        self.pos = pos

    def has_varname(self):
        return len(self.varname) > 0

    def __repr__(self):
        if self.is_propagate:
            return ".!"
        elif len(self.varname) == 0:
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
    def __init__(self, name, args, pos):
        self.name = name
        self.args = args
        self.pos = pos
        self.typ = None

    def __repr__(self):
        return f"{self.name}!({', '.join([str(a) for a in self.args])})"

    def __str__(self):
        return self.__repr__()

class SelectorExpr:
    def __init__(
        self,
        left,
        field_name,
        pos,
        field_pos,
        is_indirect=False,
        is_nonecheck=False
    ):
        self.left = left
        self.field_name = field_name
        self.field_pos = field_pos
        self.left_typ = None
        self.is_indirect = is_indirect
        self.is_nonecheck = is_nonecheck
        self.pos = pos
        self.typ = None

    def __repr__(self):
        if self.is_indirect:
            return f"{self.left}.*"
        elif self.is_nonecheck:
            return f"{self.left}.?"
        return f"{self.left}.{self.field_name}"

    def __str__(self):
        return self.__repr__()

class PathExpr:
    def __init__(self, left, field_name, pos, field_pos):
        self.left = left
        self.left_info = None
        self.field_name = field_name
        self.field_info = None
        self.field_pos = field_pos
        self.is_last = False
        self.has_error = False
        self.pos = pos
        self.typ = None

    def __repr__(self):
        return f"{self.left}::{self.field_name}"

    def __str__(self):
        return self.__repr__()

class BranchExpr:
    def __init__(self, op, pos):
        self.op = op
        self.pos = pos
        self.typ = None

    def __repr__(self):
        return str(self.op)

    def __str__(self):
        return self.__repr__()

class ReturnExpr:
    def __init__(self, expr, has_expr, pos):
        self.expr = expr
        self.has_expr = has_expr
        self.pos = pos
        self.typ = None

    def __repr__(self):
        if not self.has_expr:
            return f"return"
        return f"return {self.expr}"

    def __str__(self):
        return self.__repr__()

class RaiseExpr:
    def __init__(self, expr, pos):
        self.expr = expr
        self.pos = pos
        self.typ = None

    def __repr__(self):
        return f"raise {self.expr}"

    def __str__(self):
        return self.__repr__()

class Block:
    def __init__(self, scope, is_unsafe, stmts, expr, is_expr, pos):
        self.is_unsafe = is_unsafe
        self.stmts = stmts
        self.expr = expr
        self.is_expr = is_expr
        self.typ = None
        self.scope = scope
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
        self.branch_idx = -1 # for comptime
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
    def __init__(self, expr, branches, is_typematch, pos):
        self.expr = expr
        self.branches = branches
        self.is_typematch = is_typematch
        self.pos = pos
        self.typ = None

    def __repr__(self):
        kis = " is " if self.is_typematch else " "
        return f"match ({self.expr}){kis}{{ " + ", ".join(
            [str(b) for b in self.branches]
        ) + " }"

    def __str__(self):
        return self.__repr__()
