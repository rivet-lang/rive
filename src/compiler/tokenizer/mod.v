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
	line        int
	last_nl_pos int
	pos         int = -1

	is_started bool
	is_cr_lf   bool

	all_tokens []token.Token
	tidx       int
}

pub fn new(ctx &context.CContext) &Tokenizer {
	content := util.read_file(ctx.options.input)
	mut t := &Tokenizer{
		ctx:        ctx
		file:       ctx.options.input
		text:       content
		all_tokens: []token.Token{cap: content.len / 3}
	}
	t.tokenize_remaining_text()
	return t
}

pub fn from_memory(ctx &context.CContext, text string) &Tokenizer {
	mut t := &Tokenizer{
		ctx:        ctx
		file:       '<memory>'
		text:       text
		all_tokens: []token.Token{cap: text.len / 3}
	}
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
	for t.pos < t.text.len && t.text[t.pos] != lf {
		t.pos++
	}
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
		return t.token_eof()
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
		if util.is_valid_name(c) || c.is_digit() {
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

fn (mut t Tokenizer) read_number_mode(mode NumberMode) string {
	start := t.pos
	if mode != .dec {
		t.pos += 2 // skip '0x', '0b', '0o'
	}
	if t.pos < t.text.len && t.current_char() == num_sep {
		context.error('separator `_` is only valid between digits in a numeric literal',
			t.current_pos())
	}
	for t.pos < t.text.len {
		ch := t.current_char()
		if ch == num_sep && t.text[t.pos - 1] == num_sep {
			context.error('cannot use `_` consecutively in a numeric literal', t.current_pos())
		}
		if !mode.is_valid(ch) && ch != num_sep {
			if mode == .dec && (!ch.is_letter() || ch in [`e`, `E`]) {
				break
			} else if !ch.is_digit() && !ch.is_letter() {
				break
			}
			context.error('${mode} number has unsuitable digit `{self.current_char()}`',
				t.current_pos())
		}
		t.pos++
	}
	if t.text[t.pos - 1] == num_sep {
		t.pos--
		context.error('cannot use `_` at the end of a numeric literal', t.current_pos())
	}
	if mode != .dec && start + 2 == t.pos {
		t.pos--
		context.error('number part of this ${mode} literal is not provided', t.current_pos())
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
								context.error('number has unsuitable digit `${c}`', t.current_pos())
							}
						}
						t.pos++
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
					context.error('float literals should have a digit after the decimal point',
						t.current_pos())
					fl := t.text[start..t.pos]
					context.help('use `${fl}.0` instead of `${fl}`')
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
						context.error('this number has unsuitable digit `${c}`', t.current_pos())
					}
				}
				t.pos++
			}
		}
		if t.text[t.pos - 1] in [`e`, `E`] {
			t.pos--
			context.error('exponent has no digits', t.current_pos())
			t.pos++
		} else if t.pos < t.text.len && t.text[t.pos] == `.` && !is_range && !call_method {
			t.pos--
			if has_exp {
				context.error('exponential part should be integer', t.current_pos())
			} else {
				context.error('too many decimal points in number', t.current_pos())
			}
			t.pos++
		}
	}
	lit := t.text[start..t.pos]
	t.pos-- // fix pos
	return lit
}

fn (mut t Tokenizer) read_number() string {
	return t.read_number_mode(match true {
		t.matches('0b', t.pos) { .bin }
		t.matches('0o', t.pos) { .oct }
		t.matches('0x', t.pos) { .hex }
		else { .dec }
	})
}

fn (mut t Tokenizer) read_char() string {
	start := t.pos
	// is_bytelit := t.pos > 0 && t.text[t.pos - 1] == `b`

	mut len := 0
	for {
		t.pos++
		if t.pos >= t.text.len {
			break
		}
		if t.current_char() != backslash {
			len++
		}
		double_slash := t.matches('\\\\', t.pos - 2)
		if t.current_char() == `'` && (t.text[t.pos - 1] != backslash || double_slash) {
			if double_slash {
				len++
			}
			break
		}
	}
	len--

	ch := t.text[start + 1..t.pos]
	if len == 0 {
		context.error('empty character literal', t.current_pos())
	} else if len != 1 {
		context.error('character literal may only contain one codepoint', t.current_pos())
		context.help('if you meant to write a string literal, use double quotes')
	}
	return ch
}

fn (mut t Tokenizer) read_string() string {
	start_pos := t.current_pos()
	start := t.pos
	start_char := t.current_char()
	is_raw := t.pos > 0 && t.text[t.pos - 1] == `r`
	// is_cstr := t.pos > 0 && t.text[t.pos - 1] == `c`
	mut backslash_count := if start_char == backslash { 1 } else { 0 }
	mut n_cr_chars := 0
	for {
		t.pos++
		if t.pos >= t.text.len {
			t.pos = start
			context.error('unfinished string literal', start_pos)
			return ''
		}
		c := t.current_char()
		if c == backslash {
			backslash_count++
		}
		// end of string
		if c == `"` && (is_raw || backslash_count & 1 == 0) {
			break // handle `\\` at the end
		}
		if c == cr {
			n_cr_chars++
		}
		if c == lf {
			t.inc_line_number()
		}
		if c != backslash_count {
			backslash_count = 0
		}
	}
	mut lit := ''
	if start <= t.pos {
		lit = t.text[start + 1..t.pos]
		if n_cr_chars > 0 {
			lit = lit.replace('\r', '')
		}
	}
	return lit
}

@[inline]
fn (t &Tokenizer) token_eof() token.Token {
	return token.Token{
		kind: .eof
		pos:  t.current_pos()
	}
}

pub fn (t &Tokenizer) get_all_tokens() []token.Token {
	return t.all_tokens
}

pub fn (mut t Tokenizer) next() token.Token {
	for {
		cidx := t.tidx
		t.tidx++
		if cidx >= t.all_tokens.len {
			return t.token_eof()
		}
		return t.all_tokens[cidx]
	}
	return t.token_eof()
}

fn (mut t Tokenizer) internal_next() token.Token {
	for {
		t.pos++
		t.skip_whitespace()
		if t.pos >= t.text.len {
			return t.token_eof()
		}
		pos := t.current_pos()
		ch := t.current_char()
		nextc := t.look_ahead(1)
		if util.is_valid_name(ch) {
			lit := t.read_ident()
			return token.Token{
				lit:  lit
				kind: token.lookup(lit)
				pos:  pos
			}
		} else if ch.is_digit() {
			// decimals with 0 prefix = error
			if ch == `0` && nextc.is_digit() {
				context.error('leading zeros in decimal integer literals are not permitted',
					t.current_pos())
				context.help('use an `0o` prefix for octal integers')
			}
			return token.Token{
				lit:  t.read_number().replace('_', '')
				kind: .number
				pos:  pos
			}
		}
		match ch {
			`/` {
				if nextc == `/` {
					t.ignore_line()
					continue
				} else if nextc == `*` {
					start_pos := t.pos
					mut nest_count := 1
					t.pos++
					for nest_count > 0 && t.pos < t.text.len - 1 {
						t.pos++
						if t.pos >= t.text.len - 1 {
							old_pos := t.pos
							t.pos = start_pos
							context.error('unterminated multiline comment', t.current_pos())
							t.pos = old_pos
						}
						if t.text[t.pos] == lf {
							t.inc_line_number()
							continue
						}
						if t.matches('/*', t.pos) && t.text[t.pos + 2] != `/` {
							nest_count++
							continue
						}
						if t.matches('*/', t.pos) {
							nest_count--
						}
					}
					t.pos++
					continue
				}
			}
			`'` {
				return token.Token{
					lit:  t.read_char()
					kind: .char
					pos:  pos
				}
			}
			`"` {
				return token.Token{
					lit:  t.read_string()
					kind: .string
					pos:  pos
				}
			}
			else {
				context.error('invalid character `${ch.ascii_str()}`', pos)
				break
			}
		}
	}
	return t.token_eof()
}
