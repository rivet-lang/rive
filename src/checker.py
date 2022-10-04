# Copyright (C) 2022 The Rivet Team. All rights reserved.
# Use of this source code is governed by an MIT license
# that can be found in the LICENSE file.

from . import ast, sym, type, report, utils

class Checker:
	def __init__(self, comp):
		self.comp = comp
		self.source_file = None
		self.sym = None

	def check_files(self, source_files):
		for sf in source_files:
			self.sym = sf.sym
			self.source_file = sf
			self.check_decls(self.source_file.decls)

	def check_decls(self, decls):
		for decl in decls:
			old_sym = self.sym
			if isinstance(decl, ast.UseDecl):
				pass
			elif isinstance(decl, ast.ExternDecl):
				self.check_decls(decl.decls)
			elif isinstance(decl, ast.ModDecl):
				self.check_decls(decl.decls)
			elif isinstance(decl, ast.ConstDecl):
				pass
			elif isinstance(decl, ast.TypeDecl):
				pass
			elif isinstance(decl, ast.ErrTypeDecl):
				pass
			elif isinstance(decl, ast.EnumDecl):
				pass
			elif isinstance(decl, ast.TraitDecl):
				pass
			elif isinstance(decl, ast.SumTypeDecl):
				pass
			elif isinstance(decl, ast.ClassDecl):
				pass
			elif isinstance(decl, ast.StructDecl):
				pass
			elif isinstance(decl, ast.FieldDecl):
				pass
			elif isinstance(decl, ast.ExtendDecl):
				pass
			elif isinstance(decl, ast.FnDecl):
				pass
			elif isinstance(decl, ast.DestructorDecl):
				pass
			elif isinstance(decl, ast.TestDecl):
				pass
			self.sym = old_sym
