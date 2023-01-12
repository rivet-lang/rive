# Copyright (C) 2022 The Rivet Developers. All rights reserved.
# Use of this source code is governed by an MIT license that can
# be found in the LICENSE file.

from .sym import TypeKind
from . import ast, parser, sym, type, report, utils

class Register:
    def __init__(self, comp):
        self.comp = comp
        self.source_file = None
        self.abi = sym.ABI.Rivet
        self.sym = None

    def walk_files(self, source_files):
        for i, sf in enumerate(source_files):
            if self.comp.runtime_mod == None and sf.sym.is_runtime_mod():
                self.comp.runtime_mod = sf.sym
            self.sym = sf.sym
            self.source_file = sf
            self.walk_decls(self.source_file.decls)

    def walk_decls(self, decls):
        for decl in decls:
            old_abi = self.abi
            old_sym = self.sym
            if isinstance(decl, ast.ImportDecl):
                if len(decl.import_list) == 0:
                    if decl.vis.is_pub():
                        try:
                            self.sym.add(
                                sym.SymRef(decl.vis, decl.alias, decl.mod_sym)
                            )
                        except utils.CompilerError as e:
                            report.error(e.args[0], decl.pos)
                    else:
                        self.source_file.imported_symbols[decl.alias
                                                          ] = decl.mod_sym
                if decl.glob:
                    for symbol in decl.mod_sym.syms:
                        if not symbol.vis.is_pub():
                            continue
                        self.check_imported_symbol(symbol, decl.pos)
                        if decl.vis.is_pub():
                            try:
                                self.sym.add(
                                    sym.SymRef(decl.vis, symbol.name, symbol)
                                )
                            except utils.CompilerError as e:
                                report.error(e.args[0], decl.pos)
                        else:
                            self.source_file.imported_symbols[symbol.name
                                                              ] = symbol
                for import_info in decl.import_list:
                    if import_info.name == "self":
                        if import_info.is_pub:
                            try:
                                self.sym.add(
                                    sym.SymRef(
                                        sym.Vis.Pub, decl.alias, decl.mod_sym
                                    )
                                )
                            except utils.CompilerError as e:
                                report.error(e.args[0], decl.pos)
                        else:
                            self.source_file.imported_symbols[decl.alias
                                                              ] = decl.mod_sym
                    elif symbol := decl.mod_sym.find(import_info.name):
                        self.check_vis(symbol, import_info.pos)
                        self.check_imported_symbol(symbol, import_info.pos)
                        if import_info.is_pub:
                            try:
                                self.sym.add(
                                    sym.SymRef(
                                        sym.Vis.Pub, import_info.alias, symbol
                                    )
                                )
                            except utils.CompilerError as e:
                                report.error(e.args[0], decl.pos)
                        else:
                            self.source_file.imported_symbols[import_info.alias
                                                              ] = symbol
                    else:
                        report.error(
                            f"could not find `{import_info.name}` in module `{decl.mod_sym.name}`",
                            import_info.pos
                        )
            elif isinstance(decl, ast.ExternDecl):
                self.abi = decl.abi
                self.walk_decls(decl.decls)
            elif isinstance(decl, ast.ConstDecl):
                self.add_sym(
                    sym.Const(decl.vis, decl.name, decl.typ, decl.expr),
                    decl.pos
                )
            elif isinstance(decl, ast.LetDecl):
                for v in decl.lefts:
                    try:
                        v_sym = sym.Var(
                            decl.vis, v.is_mut, decl.is_extern, self.abi,
                            v.name, v.typ
                        )
                        self.source_file.sym.add(v_sym)
                        v.sym = v_sym
                    except utils.CompilerError as e:
                        report.error(e.args[0], v.pos)
            elif isinstance(decl, ast.TypeDecl):
                self.add_sym(
                    sym.Type(
                        decl.vis, decl.name, TypeKind.Alias,
                        info = sym.AliasInfo(decl.parent)
                    ), decl.pos
                )
            elif isinstance(decl, ast.AliasDecl):
                try:
                    if decl.is_typealias:
                        self.add_sym(
                            sym.Type(
                                decl.vis, decl.name, TypeKind.Alias,
                                info = sym.AliasInfo(decl.parent)
                            ), decl.pos
                        )
                    else:
                        # updated later
                        self.sym.add(sym.SymRef(decl.vis, decl.name, decl.parent, True))
                except utils.CompilerError as e:
                    report.error(e.args[0], decl.pos)
            elif isinstance(decl, ast.TraitDecl):
                try:
                    decl.sym = self.sym.add_and_return(
                        sym.Type(
                            decl.vis, decl.name, TypeKind.Trait,
                            info = sym.TraitInfo()
                        )
                    )
                    self.sym = decl.sym
                    self.walk_decls(decl.decls)
                except utils.CompilerError as e:
                    report.error(e.args[0], decl.pos)
            elif isinstance(decl, ast.ClassDecl):
                try:
                    is_runtime_mod = self.source_file.sym.is_runtime_mod()
                    if is_runtime_mod and decl.name == "string":
                        decl.sym = self.comp.string_t.sym
                    elif is_runtime_mod and decl.name == "Error":
                        decl.sym = self.comp.error_t.sym
                    else:
                        decl.sym = self.sym.add_and_return(
                            sym.Type(
                                decl.vis, decl.name, TypeKind.Class,
                                info = sym.ClassInfo()
                            )
                        )
                        if is_runtime_mod and decl.name == "Vec":
                            self.comp.vec_sym = decl.sym
                    self.sym = decl.sym
                    self.walk_decls(decl.decls)
                except utils.CompilerError as e:
                    report.error(e.args[0], decl.pos)
            elif isinstance(decl, ast.StructDecl):
                try:
                    decl.sym = self.sym.add_and_return(
                        sym.Type(
                            decl.vis, decl.name, TypeKind.Struct,
                            info = sym.StructInfo(decl.is_opaque)
                        )
                    )
                    self.sym = decl.sym
                    self.walk_decls(decl.decls)
                except utils.CompilerError as e:
                    report.error(e.args[0], decl.pos)
            elif isinstance(decl, ast.EnumDecl):
                try:
                    info = sym.EnumInfo(
                        decl.underlying_typ, decl.is_advanced_enum
                    )
                    for i, variant in enumerate(decl.variants):
                        if info.has_variant(variant.name):
                            report.error(
                                f"enum `{decl.name}` has duplicate variant `{variant.name}`",
                                decl.pos
                            )
                            continue
                        info.add_variant(
                            variant.name, variant.has_typ, variant.typ
                        )
                    decl.sym = self.sym.add_and_return(
                        sym.Type(
                            decl.vis, decl.name, TypeKind.Enum, info = info
                        )
                    )
                    self.sym = decl.sym
                    self.walk_decls(decl.decls)
                except utils.CompilerError as e:
                    report.error(e.args[0], decl.pos)
            elif isinstance(decl, ast.FieldDecl):
                if self.sym.has_field(decl.name):
                    report.error(
                        f"{self.sym.typeof()} `{self.sym.name}` has duplicate field `{decl.name}`",
                        decl.pos
                    )
                else:
                    self.sym.fields.append(
                        sym.Field(
                            decl.name, decl.is_mut, decl.vis, decl.typ,
                            decl.has_def_expr, decl.def_expr
                        )
                    )
            elif isinstance(decl, ast.ExtendDecl):
                if isinstance(decl.typ, type.Type):
                    if decl.typ.sym != None:
                        self.sym = decl.typ.sym
                    elif isinstance(decl.typ.expr, ast.Ident):
                        if typ_sym := self.sym.find(decl.typ.expr.name):
                            self.sym = typ_sym
                        else:
                            self.sym = self.sym.add_and_return(
                                sym.Type(
                                    sym.Vis.Priv, decl.typ.expr.name,
                                    TypeKind.Placeholder
                                )
                            )
                    else:
                        report.error(
                            f"invalid type `{decl.typ}` to extend", decl.pos
                        )
                        continue
                    self.walk_decls(decl.decls)
                else:
                    report.error(
                        f"invalid type `{decl.typ}` to extend", decl.pos
                    )
            elif isinstance(decl, ast.FnDecl):
                try:
                    decl.sym = self.sym.add_and_return(
                        sym.Fn(
                            self.abi, decl.vis, decl.is_extern, decl.is_unsafe,
                            decl.is_method, decl.is_variadic, decl.name,
                            decl.args, decl.ret_typ, decl.has_named_args,
                            decl.has_body, decl.name_pos, decl.self_is_mut,
                            decl.self_is_ref
                        )
                    )
                    decl.sym.is_main = decl.is_main
                except utils.CompilerError as e:
                    report.error(e.args[0], decl.name_pos)
            elif isinstance(decl, ast.DestructorDecl):
                self.add_sym(
                    sym.Fn(
                        self.abi, sym.Vis.Priv, False, True, True, False,
                        "_dtor", [
                            sym.Arg(
                                "self", decl.self_is_mut, type.Type(self.sym),
                                None, False, decl.pos
                            )
                        ], self.comp.void_t, False, True, decl.pos,
                        decl.self_is_mut, False
                    ), decl.pos
                )
            self.abi = old_abi
            self.sym = old_sym

    def add_sym(self, sy, pos):
        try:
            self.sym.add(sy)
        except utils.CompilerError as e:
            report.error(e.args[0], pos)

    def check_vis(self, sym_, pos):
        if sym_.vis == sym.Vis.Priv and not self.sym.has_access_to(sym_):
            report.error(f"{sym_.typeof()} `{sym_.name}` is private", pos)

    def check_imported_symbol(self, s, pos):
        if s.name in self.source_file.imported_symbols:
            report.error(f"{s.typeof()} `{s.name}` is already imported", pos)
        elif self.source_file.sym.find(s.name):
            report.error(
                f"another symbol with the name `{s.name}` already exists", pos
            )
            report.help("you can use `as` to change the name of the import")
