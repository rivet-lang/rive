// Copyright (C) 2024-present Jose Mendoza - All rights reserved. Use of this
// source code is governed by an MIT license that can be found in the LICENSE
// file.

module compiler

import compiler.context
import compiler.parser

pub fn run(args []string) {
	mut ctx := &context.CContext{}

	context.push(ctx)
	defer { context.pop() }

	ctx.options = context.parse_args(args)

	mut p := parser.new(ctx)
	p.parse()
	ctx.abort_if_errors()
}
