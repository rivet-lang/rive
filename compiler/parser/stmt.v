// Copyright (C) 2024-present The Rivet programming language. Use of this source code
// is governed by an MIT license that can be found in the LICENSE file.

module parser

import compiler.ast
import compiler.context

fn (mut p Parser) parse_block() []ast.Stmt {
	if p.tok.kind == .lbrace && p.next_tok.kind == .rbrace {
		// empty block: `{}`
		p.advance(2)
		return []
	}

	old_inside_local_scope := p.inside_local_scope
	defer { p.inside_local_scope = old_inside_local_scope }
	p.inside_local_scope = true

	mut is_finished := false
	mut stmts := []ast.Stmt{}
	lbrace_pos := p.tok.pos
	p.expect(.lbrace)
	for {
		stmts << p.parse_stmt()
		is_finished = p.accept(.rbrace)
		if is_finished || p.should_abort() {
			break
		}
	}
	if !is_finished && !p.abort {
		// we give an error because the block has not been finished (`}` was not found),
		// but it has not been aborted (due to poor formation of expressions or statements)
		context.error('unfinished block, expected `}` and found ${p.tok}', lbrace_pos)
	}
	return stmts
}

fn (mut p Parser) parse_stmt() ast.Stmt {
	// module stmts: fns, consts, vars, etc.
	is_pub := !p.inside_local_scope && p.accept(.kw_pub)
	match p.tok.kind {
		.kw_fn {
			return p.parse_fn_stmt(is_pub)
		}
		.kw_let {
			return p.parse_let_stmt(is_pub)
		}
		else {
			// local stmts: if, while, match, etc.
			if p.inside_local_scope {
				match p.tok.kind {
					.kw_while {
						return p.parse_while_stmt()
					}
					.kw_for {}
					.kw_defer {}
					else {
						// `.kw_if`, `.kw_match`, `.kw_break`, `.kw_continue` and `.kw_return` are handled in `p.parse_expr()`
						return ast.ExprStmt{p.parse_expr()}
					}
				}
				p.next()
			} else {
				context.error('invalid declaration: unexpected ${p.tok}', p.tok.pos)
				p.abort = true
			}
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
			mut arg_default_expr := ast.empty_expr
			if p.accept(.assign) {
				arg_default_expr = p.parse_expr()
			}
			args << ast.FnArg{arg_name, arg_name_pos, arg_type, arg_default_expr}
			if !p.accept(.comma) || p.should_abort() {
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

fn (mut p Parser) parse_let_stmt(is_pub bool) ast.Stmt {
	p.expect(.kw_let)
	mut lefts := []ast.Variable{}
	for {
		is_mut := p.accept(.kw_mut)
		name := p.parse_ident()
		type := if p.accept(.colon) {
			p.parse_type()
		} else {
			p.ctx.void_type
		}
		lefts << ast.Variable{
			name:     name
			is_local: p.inside_local_scope
			is_pub:   is_pub
			is_mut:   is_mut
			type:     type
		}
		if !p.accept(.comma) || p.should_abort() {
			break
		}
	}
	p.expect(.assign)
	right := p.parse_expr()
	p.expect(.semicolon)
	return ast.LetStmt{
		lefts:  lefts
		right:  right
		is_pub: is_pub
	}
}

fn (mut p Parser) parse_while_stmt() ast.Stmt {
	p.expect(.kw_while)
	p.expect(.lparen)
	mut init_stmt := ?ast.Stmt(none)
	if p.tok.kind == .kw_let {
		init_stmt = p.parse_let_stmt(false)
	}
	cond := p.parse_expr()
	mut continue_expr := ?ast.Expr(none)
	if p.accept(.semicolon) {
		continue_expr = p.parse_expr()
	}
	p.expect(.rparen)
	stmts := p.parse_block()
	return ast.WhileStmt{init_stmt, cond, continue_expr, stmts}
}
