// Copyright (C) 2024-present The Rivet programming language. Use of this source code
// is governed by an MIT license that can be found in the LICENSE file.

module parser

import compiler.ast
import compiler.token
import compiler.context
import compiler.tokenizer

pub struct Parser {
mut:
	ctx &context.CContext

	tokenizer tokenizer.Tokenizer
	prev_tok  token.Token
	tok       token.Token
	next_tok  token.Token

	file  &ast.File  = unsafe { nil }
	scope &ast.Scope = unsafe { nil }

	abort              bool
	inside_local_scope bool

	expect_is_called bool
	prev_expect_pos  ast.FilePos
}

@[inline]
pub fn new(ctx &context.CContext) &Parser {
	return &Parser{
		ctx: ctx
	}
}

pub fn (mut p Parser) parse() {
	p.parse_file(p.ctx.options.input, true)
}

fn (mut p Parser) parse_file(filename string, is_root bool) {
	p.file = ast.File.new(filename)
	if is_root {
		p.ctx.root_file = p.file
	}
	p.ctx.files << p.file

	p.tokenizer = tokenizer.from_file(p.ctx, p.file)
	if p.file.errors > 0 {
		// if the tokenizer found errors in the file, let's skip it
		return
	}

	p.advance(2)
	for {
		p.file.stmts << p.parse_stmt()
		if p.should_abort() {
			break
		}
	}
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

fn (mut p Parser) expect(kind token.Kind) {
	// this prevents an infinite loop due to an unexpected token, and also a double error message.
	if p.tok.pos == p.prev_expect_pos && !p.expect_is_called {
		p.expect_is_called = true
	} else {
		p.prev_expect_pos = p.tok.pos
	}

	if !p.accept(kind) {
		if p.expect_is_called {
			p.next()
		} else {
			context.error('expected `${kind}`, but found ${p.tok}', p.tok.pos)
		}
	}
}

fn (mut p Parser) accept(kind token.Kind) bool {
	if p.tok.kind == kind {
		p.next()
		return true
	}
	return false
}

fn (mut p Parser) parse_ident() string {
	if p.tok.kind == .ident {
		ident := p.tok.lit
		p.next()
		return ident
	}
	context.error('expected identifier, but found ${p.tok}', p.tok.pos)
	p.next()
	return ''
}

@[inline]
fn (p &Parser) should_abort() bool {
	return p.tok.kind == .eof || p.abort
}
