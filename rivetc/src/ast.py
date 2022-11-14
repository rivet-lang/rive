# Copyright (C) 2022 The Rivet Team. All rights reserved.
# Use of this source code is governed by an MIT license
# that can be found in the LICENSE file.

from enum import IntEnum as Enum, auto as auto_enum

COMPTIME_CONSTANTS = [
    "_FILE_", "_LINE_", "_COLUMN_", "_FUNCTION_", "_RIVET_VERSION_",
    "_RIVET_COMMIT_"
]

def is_comptime_constant(name):
    return name in COMPTIME_CONSTANTS

class SourceFile:
    def __init__(self, file, decls, pkg_name, sym):
        self.file = file
        self.pkg_name = pkg_name
        self.sym = sym
        self.decls = decls
        self.imported_symbols = {}

    def find_imported_symbol(self, name):
        if name in self.imported_symbols:
            return self.imported_symbols[name]
        return None

    def __repr__(self):
        return f"ast.SourceFile(file='{self.file}', pkg_name='{self.pkg_name}', decls={len(self.decls)})"

    def __str__(self):
        return self.__repr__()

# Used in `let` decls/stmts and guard exprs
class ObjDecl:
    def __init__(self, is_mut, name, has_typ, typ, level, pos):
        self.is_mut = is_mut
        self.name = name
        self.has_typ = has_typ
        self.typ = typ
        self.level = level
        self.pos = pos
        self.sym = None

    def __repr__(self):
        res = ""
        if self.is_mut:
            res += "mut "
        res += self.name
        if self.has_typ:
            res += f": {self.typ}"
        return res

    def __str__(self):
        return self.__repr__()

# ---- Declarations ----
class EmptyDecl:
    def __init__(self):
        self.attrs = Attrs()

class DocComment:
    def __init__(self, lines, pos):
        self.lines = lines
        self.pos = pos

    def is_empty(self):
        return len(self.lines) == 0

    def merge(self):
        res = ""
        for l in self.lines:
            res += l
            if len(l) == 0 or l.endswith("."):
                res += "\n"
            else:
                res += " "
        return res

class AttrArg:
    def __init__(self, name, expr):
        self.name = name
        self.expr = expr
        self.is_named = name != ""

class Attr:
    def __init__(self, name, args, pos):
        self.name = name
        self.args = args
        self.pos = pos

    def find_arg(self, name):
        for arg in self.args:
            if arg.name == name:
                return arg
        return None

class Attrs:
    def __init__(self):
        self.attrs = []

    def add(self, attr):
        self.attrs.append(attr)

    def find(self, name):
        for attr in self.attrs:
            if attr.name == name:
                return attr
        return None

    def has(self, name):
        if _ := self.find(name):
            return True
        return False

    def has_attrs(self):
        return len(self.attrs) > 0

class ImportDecl:
    def __init__(self, attrs, vis, path, alias, glob, import_list, pos):
        self.attrs = attrs
        self.vis = vis
        self.path = path
        self.alias = alias
        self.glob = glob
        self.import_list = import_list
        self.mod_sym = None
        self.pos = pos

class ImportListInfo:
    def __init__(self, name, alias, pos):
        self.name = name
        self.alias = alias
        self.pos = pos

class ImportedMod:
    def __init__(self, found, name, alias, full_name, files):
        self.found = found
        self.name = name
        self.alias = alias
        self.full_name = full_name
        self.files = files

class ExternDecl:
    def __init__(self, attrs, abi, decls, pos):
        self.attrs = attrs
        self.abi = abi
        self.decls = decls
        self.pos = pos

class ConstDecl:
    def __init__(self, docs, attrs, vis, name, typ, expr, pos):
        self.docs = docs
        self.attrs = attrs
        self.vis = vis
        self.name = name
        self.typ = typ
        self.expr = expr
        self.sym = None
        self.pos = pos

class LetDecl:
    def __init__(self, docs, attrs, vis, is_extern, abi, lefts, right, pos):
        self.docs = docs
        self.attrs = attrs
        self.vis = vis
        self.is_extern = is_extern
        self.abi = abi
        self.lefts = lefts
        self.right = right
        self.pos = pos

class TypeDecl:
    def __init__(self, docs, attrs, vis, name, parent, pos):
        self.docs = docs
        self.attrs = attrs
        self.vis = vis
        self.name = name
        self.parent = parent
        self.pos = pos

class EnumDecl:
    def __init__(
        self, docs, attrs, vis, name, underlying_typ, values, decls, pos
    ):
        self.docs = docs
        self.attrs = attrs
        self.vis = vis
        self.name = name
        self.underlying_typ = underlying_typ
        self.values = values
        self.decls = decls
        self.sym = None
        self.pos = pos

class TraitDecl:
    def __init__(self, docs, attrs, vis, name, decls, pos):
        self.docs = docs
        self.attrs = attrs
        self.vis = vis
        self.name = name
        self.decls = decls
        self.pos = pos

class ClassDecl:
    def __init__(self, docs, attrs, vis, name, bases, decls, pos):
        self.docs = docs
        self.attrs = attrs
        self.vis = vis
        self.name = name
        self.bases = bases
        self.decls = decls
        self.sym = None
        self.pos = pos

class StructDecl:
    def __init__(self, docs, attrs, vis, name, bases, decls, is_opaque, pos):
        self.docs = docs
        self.attrs = attrs
        self.vis = vis
        self.name = name
        self.bases = bases
        self.decls = decls
        self.is_opaque = is_opaque
        self.sym = None
        self.pos = pos

class FieldDecl:
    def __init__(
        self, attrs, docs, vis, is_mut, name, typ, def_expr, has_def_expr, pos
    ):
        self.docs = docs
        self.attrs = attrs
        self.vis = vis
        self.is_mut = is_mut
        self.name = name
        self.typ = typ
        self.def_expr = def_expr
        self.has_def_expr = has_def_expr
        self.pos = pos

class ExtendDecl:
    def __init__(self, attrs, typ, bases, decls, pos):
        self.attrs = attrs
        self.typ = typ
        self.bases = bases
        self.decls = decls
        self.pos = pos

class FnDecl:
    def __init__(
        self, docs, attrs, vis, is_extern, is_unsafe, name, name_pos, args,
        ret_typ, stmts, scope, has_body = False, is_method = False,
        self_is_mut = False, self_is_ref = False, has_named_args = False,
        is_main = False, is_variadic = False, abi = None
    ):
        self.sym = None
        self.docs = docs
        self.attrs = attrs
        self.vis = vis
        self.abi = abi
        self.name = name
        self.name_pos = name_pos
        self.args = args
        self.self_typ = None
        self.self_is_mut = self_is_mut
        self.self_is_ref = self_is_ref
        self.is_main = is_main
        self.is_extern = is_extern
        self.is_unsafe = is_unsafe
        self.is_method = is_method
        self.is_variadic = is_variadic
        self.ret_typ = ret_typ
        self.has_named_args = has_named_args
        self.has_body = has_body
        self.scope = scope
        self.stmts = stmts
        self.defer_stmts = []

class DestructorDecl:
    def __init__(self, self_is_mut, scope, stmts, pos):
        self.self_is_mut = self_is_mut
        self.stmts = stmts
        self.scope = scope
        self.self_typ = None
        self.pos = pos
        self.defer_stmts = []

class TestDecl:
    def __init__(self, scope, name, stmts, pos):
        self.name = name
        self.stmts = stmts
        self.scope = scope
        self.pos = pos

# ------ Statements --------
class LetStmt:
    def __init__(self, scope, lefts, right, pos):
        self.lefts = lefts
        self.right = right
        self.scope = scope
        self.pos = pos

class WhileStmt:
    def __init__(self, cond, stmt, is_inf, pos):
        self.cond = cond
        self.stmt = stmt
        self.is_inf = is_inf
        self.pos = pos

class ForStmt:
    def __init__(self, scope, vars, iterable, stmt, pos):
        self.vars = vars
        self.iterable = iterable
        self.scope = scope
        self.stmt = stmt
        self.pos = pos

class DeferStmt:
    def __init__(self, expr, is_errdefer, pos):
        self.expr = expr
        self.is_errdefer = is_errdefer
        self.flag_var = ""
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
        return f'ast.EmptyExpr(pos: "{self.pos}")'

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

class AssignExpr:
    def __init__(self, left, op, right, pos):
        self.left = left
        self.op = op
        self.right = right
        self.typ = None
        self.pos = pos

class Ident:
    def __init__(self, name, pos, scope, is_comptime):
        self.name = name
        self.obj = None
        self.sym = None
        self.is_obj = False
        self.is_sym = False
        self.is_comptime = is_comptime
        self.not_found = False
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
        self.is_mut = False
        self.typ = None
        self.pos = pos

    def __repr__(self):
        return "self"

    def __str__(self):
        return self.__repr__()

class BaseExpr:
    def __init__(self, scope, pos):
        self.scope = scope
        self.is_mut = False
        self.pos = pos
        self.typ = None

    def __repr__(self):
        return "base"

    def __str__(self):
        return self.__repr__()

class SelfTyExpr:
    def __init__(self, scope, pos):
        self.scope = scope
        self.sym = None
        self.pos = pos

    def __repr__(self):
        return "Self"

    def __str__(self):
        return self.__repr__()

class NilLiteral:
    def __init__(self, pos):
        self.pos = pos
        self.typ = None

    def __repr__(self):
        return "nil"

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
        self.typ = None

    def __repr__(self):
        return self.lit

    def __str__(self):
        return self.__repr__()

class FloatLiteral:
    def __init__(self, lit, pos):
        self.lit = lit
        self.pos = pos
        self.typ = None

    def __repr__(self):
        return self.lit

    def __str__(self):
        return self.__repr__()

class StringLiteral:
    def __init__(self, lit, is_raw, is_bytestr, is_cstr, pos):
        self.lit = lit
        self.is_raw = is_raw
        self.is_bytestr = is_bytestr
        self.is_cstr = is_cstr
        self.pos = pos
        self.typ = None

    def __repr__(self):
        p = "c" if self.is_cstr else "b" if self.is_bytestr else "r" if self.is_raw else ""
        return f'{p}"{self.lit}"'

    def __str__(self):
        return self.__repr__()

class EnumValueExpr:
    def __init__(self, value, pos):
        self.value = value
        self.sym = None
        self.pos = pos
        self.typ = None

    def __repr__(self):
        return f".{self.value}"

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

class VecLiteral:
    def __init__(self, elems, is_arr, pos):
        self.elems = elems
        self.pos = pos
        self.is_arr = is_arr
        self.typ = None

    def __repr__(self):
        if len(self.elems) == 0:
            if self.is_arr:
                return "[]!"
            return "[]"
        res = f"[{', '.join([str(e) for e in self.elems])}]"
        if self.is_arr:
            res += "!"
        return res

    def __str__(self):
        return self.__repr__()

class AsExpr:
    def __init__(self, expr, typ, pos):
        self.expr = expr
        self.pos = pos
        self.typ = typ

    def __repr__(self):
        return f"as({self.typ}, {self.expr})"

    def __str__(self):
        return self.__repr__()

class GuardExpr:
    # Examples:
    # if (var x = optional_or_result_fn()) { ... }
    # while (var byte = reader.read()) { ... }
    def __init__(self, vars, expr, has_cond, cond, scope, pos):
        self.vars = vars
        self.expr = expr
        self.has_cond = has_cond
        self.cond = cond
        self.is_result = False
        self.scope = scope
        self.pos = pos

    def __repr__(self):
        vars_str = f"{', '.join([str(v) for v in self.vars])}"
        res = f"var {vars_str} = {self.expr}"
        if self.has_cond:
            res += f"; {self.cond}"
        return res

    def __str__(self):
        return self.__repr__()

class UnaryExpr:
    def __init__(self, right, op, is_ref_mut, pos):
        self.op = op
        self.right = right
        self.right_typ = None
        self.pos = pos
        self.is_ref_mut = is_ref_mut
        self.typ = None

    def __repr__(self):
        if self.is_ref_mut:
            return f"&mut {self.right}"
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
        self.sym = None
        self.left = left
        self.args = args
        self.err_handler = err_handler
        self.is_ctor = False # Class_Struct_or_Trait(value)
        self.is_closure = False
        self.pos = pos
        self.typ = None

    def has_named_args(self):
        for arg in self.args:
            if arg.is_named:
                return True
        return False

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
        return self.err_handler.has_expr or self.err_handler.is_propagate

    def __repr__(self):
        res = f"{self.left}({', '.join([str(a) for a in self.args])})"
        if self.has_err_handler():
            res += str(self.err_handler)
        return res

    def __str__(self):
        return self.__repr__()

class CallArg:
    def __init__(self, expr, pos, name = ""):
        self.expr = expr
        self.typ = None
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
    def __init__(
        self, is_propagate, varname, expr, has_expr, varname_pos, scope, pos
    ):
        self.is_propagate = is_propagate
        self.varname = varname
        self.varname_pos = varname_pos
        self.expr = expr
        self.has_expr = has_expr
        self.scope = scope
        self.pos = pos

    def has_varname(self):
        return len(self.varname) > 0

    def __repr__(self):
        if self.is_propagate:
            return ".!"
        elif len(self.varname) == 0:
            return f" catch {self.expr}"
        return f" catch |{self.varname}| {self.expr}"

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

class RangeExpr:
    def __init__(
        self, start, end, is_inclusive, pos, has_start = True, has_end = True
    ):
        self.start = start
        self.end = end
        self.is_inclusive = is_inclusive
        self.has_start = has_start
        self.has_end = has_end
        self.pos = pos
        self.typ = None

    def __repr__(self):
        sep = "..=" if self.is_inclusive else ".."
        if self.has_start and not self.has_end:
            return f"{self.start}{sep}"
        if not self.has_start and self.has_end:
            return f"{sep}{self.end}"
        return f"{self.start}{sep}{self.end}"

    def __str__(self):
        return self.__repr__()

class SelectorExpr:
    def __init__(
        self, left, field_name, pos, field_pos, is_indirect = False,
        is_nilcheck = False
    ):
        self.left = left
        self.left_sym = None
        self.left_typ = None
        self.field_name = field_name
        self.field_is_mut = False
        self.field_pos = field_pos
        self.field_sym = None
        self.is_indirect = is_indirect
        self.is_nilcheck = is_nilcheck
        self.is_symbol_access = False
        self.not_found = False
        self.pos = pos
        self.typ = None

    def __repr__(self):
        if self.is_indirect:
            return f"{self.left}.*"
        elif self.is_nilcheck:
            return f"{self.left}.?"
        return f"{self.left}.{self.field_name}"

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
            return "return"
        return f"return {self.expr}"

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
    def __init__(self, cond, expr, is_else, op):
        self.cond = cond
        self.expr = expr
        self.is_else = is_else
        self.op = op

    def __repr__(self):
        if self.is_else:
            return f"else {self.expr}"
        return f"if {self.cond} {self.expr}"

    def __str__(self):
        return self.__repr__()

class IfExpr:
    def __init__(self, branches, has_else, pos):
        self.branches = branches
        self.has_else = has_else
        self.pos = pos
        self.typ = None

    def __repr__(self):
        return " ".join([str(b) for b in self.branches])

    def __str__(self):
        return self.__repr__()

class SwitchBranch:
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

class SwitchExpr:
    def __init__(self, expr, branches, is_typeswitch, scope, pos):
        self.expr = expr
        self.branches = branches
        self.is_typeswitch = is_typeswitch
        self.scope = scope
        self.pos = pos
        self.typ = None

    def __repr__(self):
        kis = " is " if self.is_typeswitch else " "
        return f"switch {self.expr}{kis}{{ " + ", ".join([
            str(b) for b in self.branches
        ]) + " }"

    def __str__(self):
        return self.__repr__()
