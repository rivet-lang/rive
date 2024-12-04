// Copyright (C) 2024-present Jose Mendoza - All rights reserved. Use of this
// source code is governed by an MIT license that can be found in the LICENSE
// file.

module parser

import compiler.ast
import compiler.context

fn (mut p Parser) parse_expr() ast.Expr {
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
			context.error('invalid expression', p.tok.pos)
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
