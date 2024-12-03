// Copyright (C) 2024-present Jose Mendoza - All rights reserved. Use of this
// source code is governed by an MIT license that can be found in the LICENSE
// file.

module parser

import compiler.ast
import compiler.context

fn (mut p Parser) parse_block() []ast.Stmt {
	old_inside_local_scope := p.inside_local_scope
	defer { p.inside_local_scope = old_inside_local_scope }
	p.inside_local_scope = true

	mut stmts := []ast.Stmt{}
	p.expect(.lbrace)
	for !p.accept(.rbrace) {
		stmts << p.parse_stmt()
		if p.abort {
			break
		}
	}
	return stmts
}

fn (mut p Parser) parse_stmt() ast.Stmt {
	is_pub := p.accept(.kw_pub)
	// local stmts: if, while, match, etc.
	if p.inside_local_scope {
		if is_pub {
			context.error('cannot declare public symbols inside a local scope', p.prev_tok.pos)
		}
		match p.tok.kind {
			.kw_if {}
			else {}
		}
	}
	// module stmts: fns, consts, vars, etc.
	match p.tok.kind {
		.kw_fn {
			return p.parse_fn_stmt(is_pub)
		}
		else {
			context.error('invalid statement: unexpected ${p.tok}', p.tok.pos)
			p.abort = true
		}
	}
	return ast.empty_stmt
}

fn (mut p Parser) parse_fn_stmt(is_pub bool) ast.FnStmt {
	p.expect(.kw_fn)
	name_pos := p.tok.pos
	name := p.parse_ident()
	p.expect(.lparen)
	mut args := []ast.FnArg{}
	if !p.accept(.rparen) {
		for {
			arg_name := p.parse_ident()
			arg_name_pos := p.prev_tok.pos
			p.expect(.colon)
			arg_type := p.parse_type()
			mut arg_default_expr := ast.Expr(ast.empty_expr)
			if p.accept(.eq) {
				arg_default_expr = p.parse_expr()
			}
			args << ast.FnArg{arg_name, arg_name_pos, arg_type, arg_default_expr}
			if !p.accept(.comma) || p.accept(.eof) {
				break
			}
		}
		p.expect(.rparen)
	}
	return_type := if p.tok.kind != .lbrace {
		p.parse_type()
	} else {
		p.ctx.void_type
	}
	stmts := p.parse_block()
	return ast.FnStmt{is_pub, name, name_pos, args, return_type, stmts}
}
