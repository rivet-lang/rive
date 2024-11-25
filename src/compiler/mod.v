// Copyright (C) 2024-present Jose Mendoza - All rights reserved. Use of this
// source code is governed by an MIT license that can be found in the LICENSE
// file.

module compiler

import compiler.context
import compiler.tokenizer

pub fn run(args []string) {
	mut c_ctx := &context.CContext{
		options: unsafe { nil }
	}
	context.push(c_ctx)
	defer { context.pop() }
	c_ctx.options = context.parse_args(args)
	mut t := tokenizer.new(c_ctx)
	mut tok := t.next()
	for {
		println('${tok} - ${tok.pos}')
		tok = t.next()
		if tok.kind == .eof {
			break
		}
	}
}
