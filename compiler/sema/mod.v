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
	// to generate the corresponding AST of each imported file.
	parser &parser.Parser
mut:
	ctx &context.CContext = unsafe { nil }

	file       &ast.File = unsafe { nil }
	sym        ast.Symbol
	scope      &ast.Scope = unsafe { nil }
	first_pass bool
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
	sema.ctx.universe.add_local_symbol(sema.sym) or {
		context.ic_error('cannot load module `${file.mod_name}`, there is another symbol with the same name')
	}

	sema.file.scope = ast.Scope.new(sema.ctx.universe, sema.sym)
	sema.scope = sema.file.scope

	sema.file_stmts(true)
	if sema.ctx.code_has_errors() {
		return
	}

	sema.file_stmts(false)
	if sema.ctx.code_has_errors() {
		return
	}

	sema.ctx.files << sema.file
}

fn (mut sema Sema) file_stmts(first_pass bool) {
	sema.first_pass = first_pass
	sema.stmts(mut sema.file.stmts)
}

fn (mut sema Sema) stmts(mut stmts []ast.Stmt) {
	for mut stmt in stmts {
		sema.stmt(mut stmt)
	}
}

fn (mut sema Sema) stmt(mut stmt ast.Stmt) {
	match mut stmt {
		ast.FnStmt {
			sema.fn_stmt(mut stmt)
		}
		else {}
	}
}

fn (mut sema Sema) fn_stmt(mut stmt ast.FnStmt) {
	old_scope := sema.scope
	defer {
		sema.scope = old_scope
	}

	if sema.first_pass {
		stmt.sym = &ast.Function{
			name: stmt.name
			args: stmt.args
			node: unsafe { stmt }
		}
		stmt.scope = ast.Scope.new(sema.scope, ?ast.Symbol(stmt.sym))
		sema.scope.add_local_symbol(stmt.sym) or { context.error(err.msg(), stmt.name_pos) }
		sema.scope = stmt.scope
		for arg in stmt.args {
			sema.scope.add_local_symbol(ast.Variable{
				name:     arg.name
				is_local: true
				is_arg:   true
				type:     arg.type
			}) or {
				context.error(err.msg(), arg.pos, context.note('inside function `${stmt.name}`'))
			}
		}
		sema.stmts(mut stmt.stmts)
		return
	}

	sema.scope = stmt.scope
	sema.stmts(mut stmt.stmts)
}
