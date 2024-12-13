// Copyright (C) 2024-present The Rivet programming language. Use of this source code
// is governed by an MIT license that can be found in the LICENSE file.

module parser

import compiler.ast
import compiler.context

// parses a list of statements that are enclosed in `{` `}`, it can also parse a
// single-statement if the form `: <stmt>` is used.
fn (mut p Parser) parse_stmts() []ast.Stmt {
	if p.accept(.colon) {
		// single-statement: `if (is_online): player.kick();`
		stmt := p.parse_stmt()
		p.expect_semicolon = false
		return [stmt]
	}

	p.expect_semicolon = false
	if p.tok.kind != .lbrace {
		p.abort = true
		context.error('expected block, found ${p.tok}', p.tok.pos, context.Hint{
			kind: .help
			msg:  'if you want to write a single-statement, use `:`: `if (is_online): player.kick()`'
		})
		return []
	}

	old_inside_local_scope := p.inside_local_scope
	defer { p.inside_local_scope = old_inside_local_scope }
	p.inside_local_scope = true

	stmts, _ := p.parse_simple_block()
	return stmts
}

fn (mut p Parser) parse_simple_block() ([]ast.Stmt, ?ast.Expr) {
	lbrace_pos := p.tok.pos
	if p.accept(.lbrace) && p.accept(.rbrace) {
		// empty block: `{}`
		return []ast.Stmt{}, none
	}

	mut expr := ?ast.Expr(none)

	mut is_finished := false
	mut stmts := []ast.Stmt{}
	for {
		p.expect_semicolon = true
		stmt := p.parse_stmt()
		p.expect_semicolon = false

		if p.inside_block_expr && p.tok.kind == .rbrace && stmt is ast.ExprStmt {
			// this is an expression that is being used as the value returned by the block
			expr = stmt.expr
		} else {
			stmts << stmt
		}

		is_finished = p.accept(.rbrace)
		if is_finished || p.should_abort() {
			break
		}
	}

	if !is_finished && !p.abort {
		// we give an error because the block has not been finished (`}` was not found),
		// but it has not been aborted (due to poor formation of expressions or statements)
		context.error('unfinished block, expected `}` and found ${p.tok}', lbrace_pos)
		p.abort = true
	}

	return stmts, expr
}

fn (mut p Parser) parse_stmt() ast.Stmt {
	mut old_expect_semicolon := p.expect_semicolon
	mut old_tags := p.tags
	defer {
		p.expect_semicolon = old_expect_semicolon
		p.tags = old_tags
	}

	p.tags = p.parse_tags()

	// module stmts: fns, consts, vars, etc.
	is_pub := !p.inside_local_scope && p.accept(.kw_pub)
	mut stmt := ast.empty_stmt
	match p.tok.kind {
		.kw_fn {
			stmt = p.parse_fn_stmt(is_pub)
		}
		.kw_let {
			stmt = p.parse_let_stmt(is_pub)
		}
		else {
			// local stmts: if, while, match, etc.
			if p.inside_local_scope {
				match p.tok.kind {
					.kw_for {}
					.kw_while {
						stmt = p.parse_while_stmt()
					}
					.kw_defer {
						stmt = p.parse_defer_stmt()
					}
					else {
						// `.kw_if`, `.kw_match`, `.kw_break`, `.kw_continue` and `.kw_return` are
						// handled in `p.parse_expr()`
						stmt = ast.ExprStmt{p.tags, p.parse_expr()}
					}
				}
			} else {
				context.error('invalid declaration: unexpected ${p.tok}', p.tok.pos)
				p.abort = true
			}
		}
	}
	if p.expect_semicolon && !p.should_abort() && !(p.inside_block_expr && p.tok.kind == .rbrace) {
		p.expect(.semicolon)
	}
	return stmt
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
	stmts := p.parse_stmts()
	return ast.FnStmt{p.tags, is_pub, name, name_pos, args, return_type, stmts}
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
	p.expect_semicolon = true
	return ast.LetStmt{
		tags:   p.tags
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
		p.expect(.semicolon)
	}
	cond := p.parse_expr()
	mut continue_expr := ?ast.Expr(none)
	if p.accept(.semicolon) {
		continue_expr = p.parse_expr()
	}
	p.expect(.rparen)
	stmts := p.parse_stmts()
	return ast.WhileStmt{p.tags, init_stmt, cond, continue_expr, stmts}
}

fn (mut p Parser) parse_defer_stmt() ast.Stmt {
	p.expect(.kw_defer)
	mut defer_mode := ast.DeferMode.default
	if p.accept(.lparen) {
		mode_pos := p.tok.pos
		mode := p.parse_ident()
		match mode {
			'success' {
				defer_mode = .success
			}
			'error' {
				defer_mode = .error
			}
			else {
				context.error('unknown `defer` mode', mode_pos, context.Hint{
					kind: .note
					msg:  'valid `defer` modes are `success` and `error`'
				})
			}
		}
		p.expect(.rparen)
	}
	stmts := p.parse_stmts()
	return ast.DeferStmt{
		tags:  p.tags
		mode:  defer_mode
		stmts: stmts
	}
}
