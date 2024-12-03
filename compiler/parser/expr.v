// Copyright (C) 2024-present Jose Mendoza - All rights reserved. Use of this
// source code is governed by an MIT license that can be found in the LICENSE
// file.

module parser

import compiler.ast
import compiler.context

fn (mut p Parser) parse_expr() ast.Expr {
	match p.tok.kind {
		.number {
			return p.parse_integer_lit()
		}
		.char {
			return p.parse_rune_lit()
		}
		else {
			context.error('unexpected token', p.tok.pos)
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
