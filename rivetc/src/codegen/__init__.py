# Copyright (C) 2022 The Rivet Team. All rights reserved.
# Use of this source code is governed by an MIT license
# that can be found in the LICENSE file.

import os

from ..sym import TypeKind
from ..token import Kind, OVERLOADABLE_OPERATORS_STR
from .. import ast, sym, type, prefs, colors, report, utils

from . import ir, ir_type
from .c import CBackend

def prefix_type(tt):
    prefix = ""
    if isinstance(tt, type.Ptr):
        _t = tt
        while isinstance(_t, type.Ptr):
            prefix += "ptr_"
            if tt.is_mut:
                prefix += "mut_"
            _t = _t.typ
        prefix += prefix_type(tt.typ)
    elif isinstance(tt, type.Ref):
        prefix += "ref_"
        if tt.is_mut:
            prefix += "mut_"
        prefix += prefix_type(tt.typ)
    elif isinstance(tt, type.Optional):
        prefix += "opt_" + prefix_type(tt.typ)
    return prefix

def mangle_type(typ):
    return f"{prefix_type(typ)}{mangle_symbol(typ.symbol())}"

def mangle_symbol(s):
    if len(s.mangled_name) > 0:
        return s.mangled_name
    res = []
    root = s
    while True:
        if s.is_universe:
            break
        if isinstance(s, sym.Type):
            if s.kind.is_primitive():
                name = str(s.kind)
                name = f"{len(name)}{name}"
                res.insert(0, name)
            elif s.kind == sym.TypeKind.Tuple:
                name = "Tuple_"
                for i, tt in enumerate(s.info.types):
                    name += mangle_type(tt)
                    if i < len(s.info.types) - 1:
                        name += "_"
                name = f"{len(name)}{name}"
                res.insert(0, name)
                s.mangled_name = name
            elif s.kind == sym.TypeKind.Vec:
                res.insert(0, "4core3Vec")
                s.mangled_name = "_R4core3Vec"
            elif s.kind == sym.TypeKind.Array:
                name = f"Array_{mangle_type(s.info.elem_typ)}_{s.info.size}"
                name = f"{len(name)}{name}"
                res.insert(0, name)
                s.mangled_name = name
            elif s.kind == sym.TypeKind.Enum:
                if isinstance(root, sym.Fn):
                    name = s.name
                else:
                    name = str(s.info.underlying_typ)
                name = f"{len(name)}{name}"
                res.insert(0, name)
                s.mangled_name = name
            elif s.kind == sym.TypeKind.String:
                res.insert(0, "4core6String")
                s.mangled_name = "_R4core6String"
            else:
                res.insert(0, f"{len(s.name)}{s.name}")
        elif s.name in OVERLOADABLE_OPERATORS_STR:
            name = OVERLOADABLE_OPERATORS_STR[s.name]
            name = f"{len(name)}{name}"
            res.insert(0, name)
            s.mangled_name = name
        else:
            res.insert(0, f"{len(s.name)}{s.name}")
        if s.parent == None:
            break
        else:
            s = s.parent
    res.insert(0, "_R")

    if isinstance(root, sym.Fn):
        if root.is_method:
            res.append("M")
        else:
            res.append("F")

    root.mangled_name = "".join(res)
    return root.mangled_name

class Codegen:
    def __init__(self, comp):
        self.comp = comp
        self.out_rir = ir.RIRFile(self.comp.prefs.pkg_name)

    def gen_source_files(self, source_files):
        self.gen_types()
        if report.ERRORS == 0:
            if self.comp.prefs.emit_rir:
                with open(f"{self.comp.prefs.pkg_name}.rir", "w+") as f:
                    f.write(str(self.out_rir).strip())
            if self.comp.prefs.target_backend == prefs.Backend.C:
                # self.check_pkg_attrs()
                CBackend(self.comp).gen(self.out_rir)

    def check_pkg_attrs(self):
        pkg_folder = os.path.join(
            prefs.RIVET_DIR, "objs", self.comp.prefs.pkg_name
        )
        for attr in self.pkg_attrs.attrs:
            if attr.name == "c_compile":
                if not os.path.exists(pkg_folder):
                    os.mkdir(pkg_folder)
                cfile = os.path.realpath(attr.args[0].expr.lit)
                objfile = os.path.join(
                    pkg_folder,
                    f"{os.path.basename(cfile)}.{self.get_postfix()}.o"
                )
                self.comp.prefs.objects_to_link.append(objfile)
                msg = f"c_compile: compiling object for C file `{cfile}`..."
                if os.path.exists(objfile):
                    if os.path.getmtime(objfile) < os.path.getmtime(cfile):
                        msg = f"c_compile: {objfile} is older than {cfile}, rebuilding..."
                    else:
                        continue
                self.vlog(msg)
                args = [
                    self.comp.prefs.backend_compiler, cfile, "-m64" if
                    self.comp.prefs.target_bits == prefs.Bits.X64 else "-m32",
                    "-O3" if self.comp.prefs.build_mode
                    == prefs.BuildMode.Release else "-g",
                    f'-L{os.path.dirname(cfile)}', "-c", "-o", objfile,
                ]
                res = utils.execute(*args)
                if res.exit_code != 0:
                    utils.error(
                        f"error while compiling the object file `{objfile}`:\n{res.err}"
                    )
            else:
                report.error(
                    f"unknown package attribute `{attr.name}`", attr.pos
                )
        if report.ERRORS > 0:
            self.abort()

    def get_postfix(self):
        postfix = str(self.comp.prefs.target_os).lower()
        postfix += "-"
        postfix += str(self.comp.prefs.target_arch).lower()
        postfix += "-"
        postfix += str(self.comp.prefs.target_bits).lower()
        postfix += "-"
        postfix += str(self.comp.prefs.target_endian).lower()
        postfix += "-"
        postfix += str(self.comp.prefs.target_backend).lower()
        postfix += "-"
        if self.comp.prefs.build_mode == prefs.BuildMode.Debug:
            postfix += "debug"
        else:
            postfix += "release"
        postfix += f"-{self.comp.prefs.backend_compiler}"
        return postfix

    def ir_type_from_type(self, typ):
        if isinstance(typ, type.Result):
            return ir_type.Type(f"Result_{mangle_type(typ.typ)}")
        elif isinstance(typ, type.Optional):
            if isinstance(typ.typ, type.Ref):
                return ir_type.Pointer(self.ir_type_from_type(typ.typ))
            return ir_type.Type(f"Optional_{mangle_type(typ.typ)}")
        elif isinstance(typ, type.Fn):
            args = []
            for arg in typ.args:
                args.append(self.ir_type_from_type(arg.typ))
            return ir_type.Function(args, self.ir_type_from_type(typ.ret_typ))
        elif isinstance(typ, type.Tuple):
            return ir_type.Type(mangle_symbol(typ.symbol()))
        elif isinstance(typ, type.Array):
            return ir_type.Array(typ.typ, typ.size)
        elif isinstance(typ, type.Vec):
            return ir_type.Type("_R4core3Vec")
        elif isinstance(typ, (type.Ptr, type.Ref)):
            return ir_type.Pointer(self.ir_type_from_type(typ.typ))
        return ir_type.Type(mangle_symbol(typ.symbol()))

    def gen_types(self):
        type_symbols = self.sort_type_symbols(
            self.get_type_symbols(self.comp.universe)
        )
        for ts in type_symbols:
            if ts.kind == sym.TypeKind.Tuple:
                fields = list()
                for i, f in enumerate(ts.info.types):
                    fields.append(ir.Field(f"f{i}", self.ir_type_from_type(f)))
                self.out_rir.types.append(
                    ir.Struct(False, False, False, mangle_symbol(ts), fields)
                )
            elif ts.kind == sym.TypeKind.Enum:
                for i, v in enumerate(ts.info.values):
                    v.value = i
            elif ts.kind == sym.TypeKind.Trait:
                if ts.info.has_objects:
                    ts_name = mangle_symbol(ts)
                    self.out_rir.types.append(
                        ir.Struct(
                            ts.vis.is_pub(), False, False, ts_name, [
                                ir.Field("obj", ir_type.Pointer("void")),
                                ir.Field("idx", ir_type.Type("usize"))
                            ]
                        )
                    )
                    # Virtual table
                    vtbl_name = f"{ts_name}4Vtbl"
                    static_vtbl_name = f"{ts_name}4VTBL"
                    fields = list()
                    for m in ts.syms:
                        if isinstance(m, sym.Fn):
                            fields.append(
                                ir.Field(
                                    m.name, self.ir_type_from_type(m.typ())
                                )
                            )
                    self.out_rir.types.append(
                        ir.Struct(False, False, False, vtbl_name, fields)
                    )
                    funcs = []
                    for its in ts.info.implements:
                        map = {}
                        for m in ts.syms:
                            if isinstance(m, sym.Fn):
                                if ts_method := its.find(m.name):
                                    map[m.name] = mangle_symbol(ts_method)
                                else:
                                    map[m.name] = mangle_symbol(m)
                        funcs.append(map)
                    self.out_rir.decls.append(
                        ir.VTable(
                            vtbl_name, static_vtbl_name, ts_name,
                            len(ts.info.implements), funcs
                        )
                    )
            elif ts.kind == sym.TypeKind.Class:
                fields = list()
                for f in ts.fields:
                    fields.append(
                        ir.Field(f.name, self.ir_type_from_type(f.typ))
                    )
                self.out_rir.types.append(
                    ir.Struct(
                        ts.vis.is_pub(), False, False, mangle_symbol(ts), fields
                    )
                )
            elif ts.kind == sym.TypeKind.Struct:
                fields = list()
                for f in ts.fields:
                    fields.append(
                        ir.Field(f.name, self.ir_type_from_type(f.typ))
                    )
                self.out_rir.types.append(
                    ir.Struct(
                        ts.vis.is_pub(), False, ts.info.is_opaque,
                        mangle_symbol(ts), fields
                    )
                )

    def get_type_symbols(self, root):
        ts = list()
        for s in root.syms:
            if isinstance(s, sym.Type):
                ts.append(s)
                ts += self.get_type_symbols(s)
            elif isinstance(s, sym.Pkg):
                ts += self.get_type_symbols(s)
            elif isinstance(s, sym.Mod):
                ts += self.get_type_symbols(s)
        return ts

    def sort_type_symbols(self, tss):
        dg = utils.DepGraph()
        typ_names = list()
        for ts in tss:
            if ts.kind in (sym.TypeKind.Alias, sym.TypeKind.Never):
                continue
            ts.mangled_name = mangle_symbol(ts)
            typ_names.append(ts.mangled_name)
        for ts in tss:
            if ts.kind in (sym.TypeKind.Alias, sym.TypeKind.Never):
                continue
            field_deps = list()
            if ts.kind == sym.TypeKind.Array:
                dsym = ts.info.elem_typ.symbol()
                dep = mangle_symbol(dsym)
                if dep in typ_names:
                    field_deps.append(dep)
            elif ts.kind == sym.TypeKind.Vec:
                dsym = ts.info.elem_typ.symbol()
                dep = mangle_symbol(dsym)
                if dep in typ_names:
                    field_deps.append(dep)
            elif ts.kind == sym.TypeKind.Tuple:
                for f in ts.info.types:
                    dsym = f.symbol()
                    dep = mangle_symbol(dsym)
                    if dep not in typ_names or dep in field_deps or isinstance(
                        f, type.Ref
                    ):
                        continue
                    field_deps.append(dep)
            elif ts.kind == sym.TypeKind.Class:
                for f in ts.fields:
                    dsym = f.typ.symbol()
                    dep = mangle_symbol(dsym)
                    if dep not in typ_names or dep in field_deps or isinstance(
                        f.typ, type.Optional
                    ):
                        continue
                    field_deps.append(dep)
            elif ts.kind == sym.TypeKind.Struct:
                for f in ts.fields:
                    dsym = f.typ.symbol()
                    dep = mangle_symbol(dsym)
                    if dep not in typ_names or dep in field_deps or isinstance(
                        f.typ, type.Optional
                    ):
                        continue
                    field_deps.append(dep)
            dg.add(ts.mangled_name, field_deps)
        dg_sorted = dg.resolve()
        if not dg_sorted.acyclic:
            utils.error(
                "codegen: the following types form a dependency cycle:\n" +
                dg_sorted.display_cycles()
            )
        types_sorted = list()
        for node in dg_sorted.nodes:
            for ts in tss:
                if ts.mangled_name == node.name:
                    types_sorted.append(ts)
        return types_sorted
