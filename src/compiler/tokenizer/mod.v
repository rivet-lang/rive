// Copyright (C) 2024-present Jose Mendoza - All rights reserved. Use of this
// source code is governed by an MIT license that can be found in the LICENSE
// file.

module tokenizer

import compiler.context
import compiler.token
import compiler.util
import compiler.report

const lf = 10
const cr = 13
const backslash = `\\`
const num_sep = `_`

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

pub fn from_file(ctx &context.CContext, path string) &Tokenizer {
	mut t := &Tokenizer{
		ctx:  ctx
		text: util.read_file(path)
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
	return token.Pos{t.file, t.line, int_max(1, t.current_column()), t.pos, 0}
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

fn (mut t Tokenizer) read_ident() string {
	start := t.pos
	for t.pos < t.text.len {
		c := t.text[t.pos]
		if util.is_valid_name(c) {
			t.pos++
			continue
		}
		break
	}
	lit := t.text[start..t.pos]
	t.pos--
	return lit
}

enum NumberMode {
	bin
	oct
	hex
	dec
}

@[inline]
fn (nm NumberMode) is_valid(c u8) bool {
	return match nm {
		.bin { c.is_bin_digit() }
		.oct { c.is_oct_digit() }
		.hex { c.is_hex_digit() }
		.dec { c.is_digit() }
	}
}

@[inline]
fn (nm NumberMode) str() string {
	return match nm {
		.bin { 'binary' }
		.oct { 'octal' }
		.hex { 'hexadecimal' }
		.dec { 'decimal' }
	}
}

fn (mut t Tokenizer) read_number_(mode NumberMode) string {
	start := t.pos
	if mode != .dec {
		t.pos += 2 // skip '0x', '0b', '0o'
	}
	if t.pos < t.text.len && t.current_char() == num_sep {
		report.error('separator `_` is only valid between digits in a numeric literal',
			t.current_pos())
	}
	for t.pos < t.text.len {
		ch := t.current_char()
		if ch == num_sep && t.text[t.pos - 1] == num_sep {
			report.error('cannot use `_` consecutively in a numeric literal', t.current_pos())
		}
		if !mode.is_valid(ch) && ch != num_sep {
			if mode == .dec && (!ch.is_letter() || ch in [`e`, `E`]) {
				break
			} else if !ch.is_digit() && !ch.is_letter() {
				break
			}
			report.error('${mode} number has unsuitable digit `{self.current_char()}`',
				t.current_pos())
		}
		t.pos++
	}
	if t.text[t.pos - 1] == num_sep {
		t.pos--
		report.error('cannot use `_` at the end of a numeric literal', t.current_pos())
	}
	if mode != .dec && start + 2 == t.pos {
		t.pos--
		report.error('number part of this ${mode} is not provided', t.current_pos())
		t.pos++
	}
	if mode == .dec {
		mut call_method := false // `true` for, e.g., 5.method(), 5.5.method(), 5e5.method()
		mut is_range := false // `true` for, e.g., 5..10
		// fractional part
		if t.pos < t.text.len && t.text[t.pos] == `.` {
			t.pos++
			if t.pos < t.text.len {
				// 16.6, 16.6.str()
				if t.text[t.pos].is_digit() {
					for t.pos < t.text.len {
						c := t.text[t.pos]
						if !c.is_digit() {
							if !c.is_letter() || c in [`e`, `E`] {
								// 16.6.str()
								if c == `.` && t.pos + 1 < t.text.len
									&& t.text[t.pos + 1].is_letter() {
									call_method = true
								}
								break
							} else {
								report.error('number has unsuitable digit `${c}`', t.current_pos())
							}
						}
					}
				} else if t.text[t.pos] == `.` {
					// 4.. a range
					is_range = true
					t.pos--
				} else if t.text[t.pos] in [`e`, `E`] {
					// 6.e6
				} else if t.text[t.pos].is_letter() {
					// 16.str()
					call_method = true
					t.pos--
				} else {
					// 5.
					t.pos--
					report.error('float literals should have a digit after the decimal point',
						t.current_pos())
					fl := t.text[start..t.pos]
					report.help('use `${fl}.0` instead of `${fl}`')
					t.pos++
				}
			}
		}
		// exponential part
		mut has_exp := false
		if t.pos < t.text.len && t.text[t.pos] in [`e`, `E`] {
			has_exp = true
			t.pos++
			if t.pos < t.text.len && t.text[t.pos] in [`-`, `+`] {
				t.pos++
			}
			for t.pos < t.text.len {
				c := t.text[t.pos]
				if !c.is_digit() {
					if !c.is_letter() {
						// 6e6.str()
						if c == `.` && t.pos + 1 < t.text.len && t.text[t.pos + 1].is_letter() {
							call_method = true
						}
						break
					} else {
						report.error('this number has unsuitable digit `${c}`', t.current_pos())
					}
				}
				t.pos++
			}
		}
		if t.text[t.pos - 1] in [`e`, `E`] {
			t.pos--
			report.error('exponent has no digits', t.current_pos())
			t.pos++
		} else if t.pos < t.text.len && t.text[t.pos] == `.` && !is_range && !call_method {
			t.pos--
			if has_exp {
				report.error('exponential part should be integer', t.current_pos())
			} else {
				report.error('too many decimal points in number', t.current_pos())
			}
			t.pos++
		}
	}
	lit := t.text[start..t.pos]
	t.pos-- // fix pos
	return lit
}

fn (mut t Tokenizer) read_number() string {
	return t.read_number_(match true {
		t.matches('0b', t.pos) { .bin }
		t.matches('0o', t.pos) { .oct }
		t.matches('0x', t.pos) { .hex }
		else { .dec }
	})
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
