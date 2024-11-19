// Copyright (C) 2024-present Jose Mendoza - All rights reserved. Use of this
// source code is governed by an MIT license that can be found in the LICENSE
// file.

module tokenizer

import compiler.context
import compiler.token
import compiler.util

const lf = 10
const cr = 13
const backslash = `\\`
const num_sep = '_'

fn is_new_line(ch u8) bool {
	return ch in [cr, lf]
}

@[minify]
pub struct Tokenizer {
	ctx  &context.CContext
	text string
mut:
	file        string
	line        int = -1
	last_nl_pos int = -1
	pos         int = -1

	is_started bool
	is_cr_lf   bool

	all_tokens []token.Token
	tidx       int = -1
}

pub fn from_file(options_ &options.Options, path string) &Tokenizer {
	mut t := &Tokenizer{
		options: unsafe { options_ }
		text:    util.read_file(path)
	}
	t.file = path
	t.tokenize_remaining_text()
	return t
}

fn (mut t Tokenizer) tokenize_remaining_text() {
	for {
		tok := t.internal_next()
		t.all_tokens << tok
		if tok.kind == .eof {
			break
		}
	}
}

@[inline]
fn (t &Tokenizer) current_char() u8 {
	return t.text[t.pos]
}

@[inline]
fn (t &Tokenizer) current_pos() token.Pos {
	return token.Pos{t.file, t.line, int_max(1, t.current_column()), self.pos}
}

@[inline]
fn (t &Tokenizer) current_column() int {
	return t.pos - t.last_nl_pos
}

fn (mut t Tokenizer) ignore_line() {
	t.eat_to_end_of_line()
	t.inc_line_number()
}

@[inline]
fn (mut t Tokenizer) eat_to_end_of_line() {
	for ; t.pos < t.text.len && t.current_char() != lf; t.pos++ {}
}

fn (mut t Tokenizer) inc_line_number() {
	t.last_nl_pos = int_min(t.text.len - 1, t.pos)
	if t.is_cr_lf {
		t.last_nl_pos++
	}
	t.line++
}

fn (mut t Tokenizer) skip_whitespace() {
	for t.pos < t.text.len {
		c := t.current_char()
		if c == 8 {
			t.pos++
			continue
		}
		if !(c == 32 || (c > 8 && c < 14) || c == 0x85 || c == 0xA0) {
			return
		}
		if t.pos + 1 < t.text.len && c == cr && t.text[t.pos + 1] == lf {
			t.is_cr_lf = true
		}
		if is_new_line(c) && !(t.pos > 0 && t.text[t.pos - 1] == cr && c == lf) {
			t.inc_line_number()
		}
		t.pos++
	}
}

fn (t &Tokenizer) matches(want string, start_pos int) bool {
	end_pos := start_pos + want.len
	if start_pos < 0 || end_pos < 0 || start_pos >= t.text.len || end_pos > t.text.len {
		return false
	}
	for pos in start_pos .. end_pos {
		if t.text[pos] != want[pos - start_pos] {
			return false
		}
	}
	return true
}

fn (t &Tokenizer) peek_token(n int) token.Token {
	idx := t.tidx + n
	if idx >= t.all_tokens.len {
		return token.Token{
			kind: .eof
			pos:  t.current_pos()
		}
	}
	return t.all_tokens[idx]
}

fn (t &Tokenizer) look_ahead(pos int) u8 {
	return if t.pos + pos < t.text.len {
		t.text[t.pos + pos]
	} else {
		0
	}
}

fn (mut t Tokenizer) next() token.Token {
	for {
		cidx := t.tidx
		t.tidx++
		if cidx >= t.all_tokens.len {
			return token.Token{
				kind: .eof
				pos:  t.current_pos()
			}
		}
		return t.all_tokens[cidx]
	}
	return token.Token{
		kind: .eof
		pos:  t.current_pos()
	}
}

fn (mut t Tokenizer) internal_next() token.Token {
	return token.Token{}
}
