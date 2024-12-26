// Copyright (C) 2024-present The Rivet programming language. Use of this source code
// is governed by an MIT license that can be found in the LICENSE file.

module sema

import compiler.ast
import compiler.parser
import compiler.context

pub struct Sema {
pub:
	// Because other files can be imported using the builtin
	// function `import`, we need to have access to the parser
	// to generate the corresponding AST of each imported file
	parser &parser.Parser
mut:
	ctx &context.CContext = unsafe { nil }

	file  &ast.File = unsafe { nil }
	sym   ast.Symbol
	scope &ast.Scope = unsafe { nil }
}

pub fn (mut sema Sema) analyze(ctx &context.CContext) {
	sema.ctx = ctx
	sema.ctx.load_builtin_symbols()
	sema.check_file(mut sema.ctx.root_file)
}

fn (mut sema Sema) check_file(mut file ast.File) {
	sema.file = file
	sema.sym = ast.TypeSym{
		name: sema.file.mod_name
		kind: .struct
	}
	sema.ctx.universe.add_symbol(sema.sym) or { context.error(err.msg(), sema.file.pos) }
	sema.file.scope = ast.Scope.new(sema.ctx.universe, sema.sym)
	sema.scope = sema.file.scope

	sema.ctx.files << sema.file
}
