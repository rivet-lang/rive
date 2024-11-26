// Copyright (C) 2024-present Jose Mendoza - All rights reserved. Use of this
// source code is governed by an MIT license that can be found in the LICENSE
// file.

module parser

import compiler.ast
import compiler.token
import compiler.context
import compiler.tokenizer

pub struct Parser {
mut:
	ctx &context.CContext

	file &ast.File = unsafe { nil }

	tokenizer tokenizer.Tokenizer
	prev_tok  token.Token
	tok       token.Token
	next_tok  token.Token
}

pub fn new(ctx &context.CContext) &Parser {
	return &Parser{
		ctx: ctx
	}
}

pub fn (mut p Parser) parse() {
	p.parse_file(p.ctx.options.input)
}

fn (mut p Parser) parse_file(file string) {
	p.file = ast.File.new(file)
	p.ctx.files << p.file

	p.tokenizer = tokenizer.from_file(p.ctx, p.file)
	p.advance(3)
}

fn (mut p Parser) next() {
	p.prev_tok = p.tok
	p.tok = p.next_tok
	p.next_tok = p.tokenizer.next()
}

fn (mut p Parser) advance(n int) {
	for _ in 0 .. n {
		p.next()
	}
}
