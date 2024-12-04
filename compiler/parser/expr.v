// Copyright (C) 2024-present The Rivet programming language. Use of this source code
// is governed by an MIT license that can be found in the LICENSE file.

module parser

import compiler.ast
import compiler.context

fn (mut p Parser) parse_expr() ast.Expr {
	if p.should_abort() {
		return ast.empty_expr
	}
	return p.parse_or_expr()
}

fn (mut p Parser) parse_or_expr() ast.Expr {
	mut left := p.parse_and_expr()
	for p.accept(.log_or) {
		right := p.parse_and_expr()
		left = ast.BinaryExpr{
			left:  left
			op:    .log_or
			right: right
			pos:   left.pos.extend(right.pos)
		}
	}
	return left
}

fn (mut p Parser) parse_and_expr() ast.Expr {
	mut left := p.parse_equality_expr()
	for p.accept(.log_and) {
		right := p.parse_equality_expr()
		left = ast.BinaryExpr{
			left:  left
			op:    .log_and
			right: right
			pos:   left.pos.extend(right.pos)
		}
	}
	return left
}

fn (mut p Parser) parse_equality_expr() ast.Expr {
	mut left := p.parse_relational_expr()
	for p.tok.kind in [.eq, .ne] {
		op := p.tok.kind
		p.next()
		right := p.parse_relational_expr()
		left = ast.BinaryExpr{
			left:  left
			op:    if op == .eq { .eq } else { .ne }
			right: right
			pos:   left.pos.extend(right.pos)
		}
	}
	return left
}

fn (mut p Parser) parse_relational_expr() ast.Expr {
	mut left := p.parse_shift_expr()
	for p.tok.kind in [.gt, .lt, .le, .or_else, .kw_in, .not_in, .kw_is, .not_is] {
		op := p.tok.kind
		p.next()
		right := p.parse_shift_expr()
		left = ast.BinaryExpr{
			left:  left
			op:    match op {
				.gt { .gt }
				.lt { .lt }
				.ge { .ge }
				.or_else { .or_else }
				.kw_in { .kw_in }
				.kw_is { .kw_is }
				else { .unknown }
			}
			right: right
			pos:   left.pos.extend(right.pos)
		}
	}
	return left
}

fn (mut p Parser) parse_shift_expr() ast.Expr {
	mut left := p.parse_additive_expr()
	for p.tok.kind in [.amp, .pipe, .xor, .lshift, .rshift] {
		op := p.tok.kind
		p.next()
		right := p.parse_additive_expr()
		left = ast.BinaryExpr{
			left:  left
			op:    match op {
				.amp { .amp }
				.pipe { .pipe }
				.xor { .xor }
				.lshift { .lshift }
				.rshift { .rshift }
				else { .unknown }
			}
			right: right
			pos:   left.pos.extend(right.pos)
		}
	}
	return left
}

fn (mut p Parser) parse_additive_expr() ast.Expr {
	mut left := p.parse_multiplicative_expr()
	for p.tok.kind in [.plus, .minus] {
		op := p.tok.kind
		p.next()
		right := p.parse_multiplicative_expr()
		left = ast.BinaryExpr{
			left:  left
			op:    match op {
				.plus { .plus }
				.minus { .minus }
				else { .unknown }
			}
			right: right
			pos:   left.pos.extend(right.pos)
		}
	}
	return left
}

fn (mut p Parser) parse_multiplicative_expr() ast.Expr {
	mut left := p.parse_unary_expr()
	for p.tok.kind in [.mul, .div, .mod] {
		op := p.tok.kind
		p.next()
		right := p.parse_unary_expr()
		left = ast.BinaryExpr{
			left:  left
			op:    match op {
				.mul { .mul }
				.div { .div }
				.mod { .mod }
				else { .unknown }
			}
			right: right
			pos:   left.pos.extend(right.pos)
		}
	}
	return left
}

fn (mut p Parser) parse_unary_expr() ast.Expr {
	mut expr := ast.empty_expr
	if p.tok.kind in [.amp, .bang, .bit_not, .minus] {
		op := p.tok.kind
		pos := p.tok.pos
		p.next()
		right := p.parse_unary_expr()
		expr = ast.UnaryExpr{
			right: expr
			op:    match op {
				.amp { .amp }
				.bang { .bang }
				.bit_not { .bit_not }
				.minus { .minus }
				else { .unknown }
			}
			pos:   pos.extend(right.pos)
		}
	} else {
		expr = p.parse_primary_expr()
	}
	return expr
}

fn (mut p Parser) parse_primary_expr() ast.Expr {
	match p.tok.kind {
		.kw_if {}
		.kw_match {}
		.kw_break {}
		.kw_continue {}
		.kw_return {}
		.number {
			return p.parse_integer_lit()
		}
		.char {
			return p.parse_rune_lit()
		}
		else {
			context.error('invalid expression: unexpected ${p.tok}', p.tok.pos)
			p.abort = true
		}
	}
	return ast.empty_expr
}

fn (mut p Parser) parse_integer_lit() ast.IntegerLit {
	return ast.IntegerLit{}
}

fn (mut p Parser) parse_rune_lit() ast.RuneLit {
	return ast.RuneLit{}
}
