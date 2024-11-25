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

	source_file &ast.SourceFile = unsafe { nil }

	tokenizer tokenizer.Tokenizer
	prev_tok  token.Token
	tok       token.Token
	peek_tok  token.Token
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
	p.source_file = ast.SourceFile.new(file)
	p.tokenizer = tokenizer.from_source_file(p.ctx, p.source_file)
	p.advance(3)

	p.ctx.source_files << p.source_file
}

fn (mut p Parser) next() {
	p.prev_tok = p.tok
	p.tok = p.peek_tok
	p.peek_tok = p.tokenizer.next()
}

fn (mut p Parser) advance(n int) {
	for _ in 0 .. n {
		p.next()
	}
}
