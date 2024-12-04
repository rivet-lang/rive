// Copyright (C) 2024-present The Rivet programming language. Use of this source code
// is governed by an MIT license that can be found in the LICENSE file.

module parser

import compiler.ast
import compiler.context

fn (mut p Parser) parse_type() ast.Type {
	if p.tok.kind == .ident {
		defer { p.next() }
		match p.tok.lit {
			'never' { return p.ctx.never_type }
			'i8' { return p.ctx.i8_type }
			'i16' { return p.ctx.i16_type }
			'i32' { return p.ctx.i32_type }
			'i64' { return p.ctx.i64_type }
			'int' { return p.ctx.int_type }
			'u8' { return p.ctx.u8_type }
			'u16' { return p.ctx.u16_type }
			'u32' { return p.ctx.u32_type }
			'u64' { return p.ctx.u64_type }
			'uint' { return p.ctx.uint_type }
			'f32' { return p.ctx.f32_type }
			'f64' { return p.ctx.f64_type }
			'bool' { return p.ctx.bool_type }
			'rune' { return p.ctx.rune_type }
			else {}
		}
	}
	expr := p.parse_expr()
	return ast.UnresolvedType{expr, expr.pos}
}
