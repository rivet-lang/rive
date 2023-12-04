# Copyright (C) 2023 The Rivet Developers. All rights reserved.
# Use of this source code is governed by an MIT license that can
# be found in the LICENSE file.

from enum import IntEnum as Enum, auto as auto_enum

from .. import utils
from ..token import Kind

def get_ir_op(op):
    if op == Kind.Plus:
        op_kind = InstKind.Add
    elif op == Kind.Minus:
        op_kind = InstKind.Sub
    elif op == Kind.Mul:
        op_kind = InstKind.Mult
    elif op == Kind.Div:
        op_kind = InstKind.Div
    elif op == Kind.Mod:
        op_kind = InstKind.Mod
    elif op == Kind.Amp:
        op_kind = InstKind.BitAnd
    elif op == Kind.Pipe:
        op_kind = InstKind.BitOr
    elif op == Kind.Xor:
        op_kind = InstKind.BitXor
    elif op == Kind.Lshift:
        op_kind = InstKind.Lshift
    elif op == Kind.Rshift:
        op_kind = InstKind.Rshift
    else:
        return None
    return op_kind

class Type:
    def __init__(self, name):
        self.name = name

    def ptr(self, is_managed = False):
        return Pointer(self, is_managed)

    def __repr__(self):
        return str(self)

    def __str__(self):
        return self.name

    def __eq__(self, other):
        return str(self) == str(other)

class Pointer:
    def __init__(self, typ, is_managed = False):
        self.typ = typ
        self.is_managed = is_managed

    def ptr(self, is_managed = False):
        return Pointer(self, is_managed)

    def nr_level(self):
        nr = 0
        ptr = self
        while isinstance(ptr, Pointer):
            ptr = ptr.typ
            nr += 1
        return nr

    def __repr__(self):
        return str(self)

    def __str__(self):
        if self.is_managed:
            return f"+{self.typ}"
        return f"*{self.typ}"

    def __eq__(self, other):
        return str(self) == str(other)

VOID_T = Type("void")
VOID_PTR_T = VOID_T.ptr()
BOOL_T = Type("bool")
RUNE_T = Type("rune")
C_INT_T = Type("int")
CHAR_T = Type("char")
UINT8_T = Type("uint8")
UINT64_T = Type("uint64")
Float64_T = Type("float64")
INT_T = Type("ri_int")
UINT_T = Type("ri_uint")
DYN_ARRAY_T = Type("_R4core8DynArray")
STRING_T = Type("_R4core6string")
TEST_T = Type("_R4core4Test")
TEST_RUNNER_T = Type("_R4core10TestRunner")

class Array:
    def __init__(self, typ, size):
        self.typ = typ
        self.size = size

    def ptr(self):
        return Pointer(self)

    def __repr__(self):
        return str(self)

    def __str__(self):
        return f"{[self.size]}{self.typ}"

    def __eq__(self, other):
        return str(self) == str(other)

class Function:
    def __init__(self, args, ret_typ):
        self.args = args
        self.ret_typ = ret_typ

    def ptr(self):
        return Pointer(self)

    def __repr__(self):
        return str(self)

    def __str__(self):
        return f"*func({', '.join([str(arg) for arg in self.args])}) {self.ret_typ}"

    def __eq__(self, other):
        return str(self) == str(other)

class RIRFile:
    def __init__(self, mod_name):
        self.mod_name = mod_name
        self.structs = []
        self.externs = []
        self.globals = []
        self.decls = []

    def __repr__(self):
        sb = utils.Builder()
        sb.writeln(
            f"// Rivet Intermediate Representation for module `{self.mod_name}`."
        )
        sb.writeln(f"// Auto-generated by {utils.full_version()}.\n//")
        sb.writeln(
            "// WARNING: This output format is intended for human consumers only"
        )
        sb.writeln(
            "// and is subject to change without notice. Knock yourself out."
        )
        sb.writeln()
        for i, s in enumerate(self.structs):
            sb.writeln(str(s))
            if i < len(self.structs) - 1:
                sb.writeln()
        sb.writeln()
        for i, e in enumerate(self.externs):
            sb.write(str(e))
        sb.writeln()
        for i, g in enumerate(self.globals):
            sb.writeln(str(g))
        sb.writeln()
        for i, d in enumerate(self.decls):
            sb.writeln(str(d))
            if i < len(self.decls) - 1:
                sb.writeln()
        return str(sb)

    def __str__(self):
        return self.__repr__()

class VTable:
    def __init__(self, structure, name, trait_name, implement_nr, funcs):
        self.structure = structure
        self.name = name
        self.trait_name = trait_name
        self.implement_nr = implement_nr
        self.funcs = funcs

    def __str__(self):
        sb = utils.Builder()
        sb.writeln(f'virtual_table {self.trait_name} {{')
        for i, ft in enumerate(self.funcs):
            sb.writeln(f'  {i} {{')
            for f, impl in ft.items():
                sb.writeln(f'    {f}: {impl}')
            sb.write("  }")
            if i < len(self.funcs) - 1:
                sb.writeln(",")
            else:
                sb.writeln()
        sb.write("}")
        return str(sb)

class Struct:
    def __init__(self, is_opaque, name, fields):
        self.is_opaque = is_opaque
        self.name = name
        self.fields = fields

    def __str__(self):
        sb = utils.Builder()
        if self.is_opaque:
            sb.write(f'type {self.name} opaque')
        else:
            sb.writeln(f'type {self.name} {{')
            for i, f in enumerate(self.fields):
                sb.write(f'  {f.name}: {f.typ}')
                if i < len(self.fields) - 1:
                    sb.writeln(",")
                else:
                    sb.writeln()
            sb.write("}")
        return str(sb)

class Field:
    def __init__(self, name, typ):
        self.name = name
        self.typ = typ

class GlobalVar:
    def __init__(self, is_public, is_extern, typ, name):
        self.is_public = is_public
        self.is_extern = is_extern
        self.typ = typ
        self.name = name

    def __str__(self):
        if self.is_public:
            kw = "export "
        elif self.is_extern:
            kw = "extern "
        else:
            kw = ""
        return f'{kw}var %{self.name}: {self.typ}'

class Local:
    def __init__(self, name, typ):
        self.name = name
        self.typ = typ

class FuncDecl:
    def __init__(
        self, is_public, attrs, is_extern, name, args, is_variadic, ret_typ,
        is_never
    ):
        self.is_public = is_public
        self.attrs = attrs
        self.is_extern = is_extern
        self.name = name
        self.args = args
        self.is_variadic = is_variadic
        self.ret_typ = ret_typ
        self.is_never = is_never
        self.arr_ret_struct = ""

        self.locals = []
        self.locals_nr = 0
        self.uniq_ids = 0
        self.instrs = list()

    def add_comment(self, comment):
        self.instrs.append(Comment(comment))

    def local_name(self):
        name = f"_{self.locals_nr}_"
        self.locals_nr += 1
        return name

    def add_label(self, label):
        self.instrs.append(Label(label))

    def add_and_get_label(self):
        label = self.local_name()
        self.add_label(label)
        return label

    def breakpoint(self):
        self.add_inst(Inst(InstKind.Breakpoint, []))

    def inline_alloca(self, typ, var, val = None):
        self.add_local(var, typ)
        if val:
            self.add_inst(Inst(InstKind.Alloca, [Ident(typ, var), val]))
        else:
            self.add_inst(Inst(InstKind.Alloca, [Ident(typ, var)]))

    def alloca(self, var, val = None):
        self.add_local(var.name, var.typ)
        if val:
            self.add_inst(Inst(InstKind.Alloca, [var, val]))
        else:
            self.add_inst(Inst(InstKind.Alloca, [var]))

    def store(self, var, val):
        self.add_inst(Inst(InstKind.Store, [var, val]))

    def store_ptr(self, var, val):
        self.add_inst(Inst(InstKind.StorePtr, [var, val]))

    def add_inst(self, inst):
        self.instrs.append(inst)

    def add_br(self, label):
        self.add_inst(Inst(InstKind.Br, [Name(label)]))

    def add_cond_single_br(self, cond, label1):
        self.add_inst(Inst(InstKind.Br, [cond, Name(label1)]))

    def add_cond_br(self, cond, label1, label2):
        self.add_inst(Inst(InstKind.Br, [cond, Name(label1), Name(label2)]))

    def add_call(self, name, args = list()):
        args_ = [Name(name), *args]
        self.add_inst(Inst(InstKind.Call, args_))

    def add_ret(self, expr):
        self.add_inst(Inst(InstKind.Ret, [expr]))

    def add_ret_void(self):
        self.add_inst(Inst(InstKind.Ret, []))

    def add_dbg_stmt_line(self, pos):
        self.add_inst(
            Inst(
                InstKind.DbgStmtLine,
                [Name(pos.file), Name(str(pos.line + 1))]
            )
        )

    def add_local(self, name, typ):
        if self.exists_local(name):
            raise Exception(f"{self.name}: duplicate local name `{name}`")
        self.locals.append(Local(name, typ))

    def exists_local(self, name):
        for local in self.locals:
            if local.name == name:
                return True
        return False

    def unique_name(self, name):
        if self.exists_local(name):
            id = self.uniq_ids
            self.uniq_ids += 1
            return f"{name}_{id}"
        return name

    def __str__(self):
        sb = utils.Builder()
        if self.is_extern:
            sb.write("extern ")
        elif self.is_public:
            sb.write("export ")
        sb.write(f'func {self.name}(')
        for i, arg in enumerate(self.args):
            sb.write(f'%{arg.name}: {arg.typ}')
            if i < len(self.args) - 1:
                sb.write(", ")
        if self.is_variadic:
            if len(self.args) > 0:
                sb.write(", ")
            sb.write("...")
        sb.write(f") {self.ret_typ}")
        if self.is_extern:
            sb.writeln("")
        else:
            sb.writeln(" {")
            for i in self.instrs:
                if isinstance(i, Label):
                    sb.writeln()
                else:
                    sb.write("  ")
                sb.writeln(str(i))
            sb.write("}")
        return str(sb)

class Comment:
    def __init__(self, text):
        self.text = text

    def __repr__(self):
        return f"// {self.text}"

    def __str__(self):
        return self.__repr__()

class NoneLit:
    def __init__(self, typ):
        self.typ = typ

    def __repr__(self):
        return "none"

    def __str__(self):
        return self.__repr__()

class IntLit:
    def __init__(self, typ, lit):
        self.typ = typ
        self.lit = lit

    def value(self):
        return int(self.lit, 0)

    def __repr__(self):
        return f"{self.typ} {self.lit}"

    def __str__(self):
        return self.__repr__()

class FloatLit:
    def __init__(self, typ, lit):
        self.typ = typ
        self.lit = lit

    def __repr__(self):
        return f"{self.typ} {self.lit}"

    def __str__(self):
        return self.__repr__()

class RuneLit:
    def __init__(self, typ, lit):
        self.lit = lit
        self.typ = typ

    def __repr__(self):
        return f"rune '{self.lit}'"

    def __str__(self):
        return self.__repr__()

class StringLit:
    def __init__(self, lit, len_):
        self.lit = lit
        self.len = len_
        self.typ = Type("uint8").ptr()

    def __repr__(self):
        return f'uint8* "{self.lit}"'

    def __str__(self):
        return self.__repr__()

class ArrayLit:
    def __init__(self, typ, elems):
        self.typ = typ
        self.elems = elems

    def __repr__(self):
        return f"{self.typ} [{', '.join([str(e) for e in self.elems])}]"

    def __str__(self):
        return self.__repr__()

class Ident: # Local and global values
    def __init__(self, typ, name):
        self.name = name
        self.typ = typ

    def __repr__(self):
        return f'%{self.name}'

    def __str__(self):
        return self.__repr__()

class Selector:
    def __init__(self, typ, left, name):
        self.typ = typ
        self.left = left
        self.name = name

    def __repr__(self):
        return f'{self.left}.{self.name}'

    def __str__(self):
        return self.__repr__()

class Name: # Simple identifier, e.g. labels
    def __init__(self, name):
        self.name = name

    def __repr__(self):
        return self.name

    def __str__(self):
        return self.name

class Label:
    def __init__(self, label):
        self.label = label

    def __repr__(self):
        return f"{self.label}:"

    def __str__(self):
        return self.__repr__()

class Skip:
    def __init__(self):
        self.typ = Type("void")

    def __repr__(self):
        return "<skip>"

    def __str__(self):
        return self.__repr__()

class InstKind(Enum):
    Nop = auto_enum()
    Alloca = auto_enum()
    Store = auto_enum()
    StorePtr = auto_enum()
    LoadPtr = auto_enum()
    GetElementPtr = auto_enum()
    GetRef = auto_enum()

    Cast = auto_enum()
    Cmp = auto_enum()

    DbgStmtLine = auto_enum()
    Breakpoint = auto_enum()

    # arithmetic operators
    Add = auto_enum()
    Sub = auto_enum()
    Mult = auto_enum()
    Div = auto_enum()
    Mod = auto_enum()
    Inc = auto_enum()
    Dec = auto_enum()

    # unary operators
    Neg = auto_enum()
    BitNot = auto_enum()

    # boolean operators
    BooleanNot = auto_enum()

    # bitwise operators
    BitAnd = auto_enum()
    BitOr = auto_enum()
    BitXor = auto_enum()
    Lshift = auto_enum()
    Rshift = auto_enum()

    # routine operators
    Br = auto_enum()
    Call = auto_enum()
    Ret = auto_enum()

    def __repr__(self):
        if self == InstKind.Alloca: return "alloca"
        elif self == InstKind.Store: return "store"
        elif self == InstKind.StorePtr: return "store_ptr"
        elif self == InstKind.LoadPtr: return "load_ptr"
        elif self == InstKind.GetElementPtr: return "get_element_ptr"
        elif self == InstKind.GetRef: return "get_ref"
        elif self == InstKind.Cast: return "cast"
        elif self == InstKind.Cmp: return "cmp"
        elif self == InstKind.DbgStmtLine: return "dbg_stmt_line"
        elif self == InstKind.Breakpoint: return "breakpoint"
        elif self == InstKind.Add: return "add"
        elif self == InstKind.Sub: return "sub"
        elif self == InstKind.Mult: return "mult"
        elif self == InstKind.Div: return "div"
        elif self == InstKind.Mod: return "mod"
        elif self == InstKind.Inc: return "inc"
        elif self == InstKind.Dec: return "dec"
        elif self == InstKind.Neg: return "neg"
        elif self == InstKind.BitNot: return "bit_not"
        elif self == InstKind.BooleanNot: return "boolean_not"
        elif self == InstKind.BitAnd: return "bit_and"
        elif self == InstKind.BitOr: return "bit_or"
        elif self == InstKind.BitXor: return "bit_xor"
        elif self == InstKind.Lshift: return "lshift"
        elif self == InstKind.Rshift: return "rshift"
        elif self == InstKind.Br: return "br"
        elif self == InstKind.Call: return "call"
        elif self == InstKind.Ret: return "ret"
        return "nop"

    def __str__(self):
        return self.__repr__()

class Inst:
    def __init__(self, kind, args, typ = Type("void")):
        self.kind = kind
        self.args = args
        self.typ = typ

    def __repr__(self):
        if self.kind == InstKind.Alloca:
            if len(self.args) == 1:
                return f"%{self.args[0].name}: {self.args[0].typ}"
            return f"%{self.args[0].name}: {self.args[0].typ} = {self.args[1]}"
        if self.kind == InstKind.Call:
            return f'{self.kind} {self.args[0]}({", ".join([str(arg) for arg in self.args[1:]])})'
        if self.kind == InstKind.Cast:
            return f"{self.kind} {self.args[0]} as {self.args[1]}"
        return f"{self.kind} {', '.join([str(arg) for arg in self.args])}"

    def __str__(self):
        return self.__repr__()
