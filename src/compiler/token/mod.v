// Copyright (C) 2024-present Jose Mendoza - All rights reserved. Use of this
// source code is governed by an MIT license that can be found in the LICENSE
// file.

module token

pub struct Pos {
pub:
	file    string
	line_nr int // the line number in the source where the token occurred
	col     int // the column in the source where the token occurred
	pos     int // the position of the token in scanner text
	len     int // length of the literal
}

@[minify]
pub struct Token {
pub:
	kind Kind   // the token number/enum; for quick comparisons
	lit  string // literal representation of the token
	tidx int    // the index of the token
	pos  Pos
}

pub enum Kind {
	unknown
	eof
	ident                       // foo
	number                      // 123
	string                      // "foo"
	char                        // 'A'
	plus                        // +
	minus                       // -
	mul                         // *
	div                         // /
	mod                         // %
	xor                         // ^
	pipe                        // |
	inc                         // ++
	dec                         // --
	log_and                     // &&
	log_or                      // ||
	bang                        // !
	bit_not                     // ~
	question                    // ?
	comma                       // ,
	semicolon                   // ;
	colon                       // :
	amp                         // &
	hash                        // #
	dollar                      // $
	at                          // @
	left_shift                  // <<
	right_shift                 // >>
	not_in                      // !in
	not_is                      // !is
	assign                      // =
	decl_assign                 // :=
	plus_assign                 // +=
	minus_assign                // -=
	div_assign                  // /=
	mult_assign                 // *=
	xor_assign                  // ^=
	mod_assign                  // %=
	or_assign                   // |=
	and_assign                  // &=
	right_shift_assign          // <<=
	left_shift_assign           // >>=
	unsigned_right_shift_assign // >>>=
	boolean_and_assign          // &&=
	boolean_or_assign           // ||=
	lcbr                        // {
	rcbr                        // }
	lpar                        // (
	rpar                        // )
	lsbr                        // [
	rsbr                        // ]
	eq       // ==
	ne       // !=
	gt       // >
	lt       // <
	ge       // >=
	le       // <=
	dot      // .
	dotdot   // ..
	ellipsis // ...

	keyword_beg
	key_as
	key_break
	key_const
	key_continue
	key_defer
	key_else
	key_enum
	key_false
	key_for
	key_fn
	key_if
	key_in
	key_is
	key_match
	key_mut
	key_none
	key_record
	key_return
	key_trait
	key_true
	key_union
	key_pub
	key_unsafe
	keyword_end

	_end_
}

pub const token_str = build_token_str()
pub const keywords = build_keys()

pub const assign_tokens = [Kind.assign, .decl_assign, .plus_assign, .minus_assign, .mult_assign,
	.div_assign, .xor_assign, .mod_assign, .or_assign, .and_assign, .right_shift_assign,
	.left_shift_assign, .unsigned_right_shift_assign, .boolean_and_assign, .boolean_or_assign]

fn build_keys() map[string]Kind {
	mut res := map[string]Kind{}
	for t in int(Kind.keyword_beg) + 1 .. int(Kind.keyword_end) {
		res[token_str[t]] = unsafe { Kind(t) }
	}
	return res
}

fn build_token_str() []string {
	mut s := []string{len: int(Kind._end_)}
	s[Kind.unknown] = 'unknown'
	s[Kind.eof] = 'eof'
	s[Kind.ident] = 'ident'
	s[Kind.number] = 'number'
	s[Kind.string] = 'string'
	s[Kind.char] = 'char'
	s[Kind.plus] = '+'
	s[Kind.minus] = '-'
	s[Kind.mul] = '*'
	s[Kind.div] = '/'
	s[Kind.mod] = '%'
	s[Kind.xor] = '^'
	s[Kind.bit_not] = '~'
	s[Kind.pipe] = '|'
	s[Kind.hash] = '#'
	s[Kind.amp] = '&'
	s[Kind.inc] = '++'
	s[Kind.dec] = '--'
	s[Kind.log_and] = '&&'
	s[Kind.log_or] = '||'
	s[Kind.bang] = '!'
	s[Kind.dot] = '.'
	s[Kind.dotdot] = '..'
	s[Kind.ellipsis] = '...'
	s[Kind.comma] = ','
	s[Kind.not_in] = '!in'
	s[Kind.not_is] = '!is'
	s[Kind.semicolon] = ';'
	s[Kind.colon] = ':'
	s[Kind.assign] = '='
	s[Kind.decl_assign] = ':='
	s[Kind.plus_assign] = '+='
	s[Kind.minus_assign] = '-='
	s[Kind.mult_assign] = '*='
	s[Kind.div_assign] = '/='
	s[Kind.xor_assign] = '^='
	s[Kind.mod_assign] = '%='
	s[Kind.or_assign] = '|='
	s[Kind.and_assign] = '&='
	s[Kind.right_shift_assign] = '>>='
	s[Kind.unsigned_right_shift_assign] = '>>>='
	s[Kind.left_shift_assign] = '<<='
	s[Kind.boolean_or_assign] = '||='
	s[Kind.boolean_and_assign] = '&&='
	s[Kind.lcbr] = '{'
	s[Kind.rcbr] = '}'
	s[Kind.lpar] = '('
	s[Kind.rpar] = ')'
	s[Kind.lsbr] = '['
	s[Kind.rsbr] = ']'
	s[Kind.eq] = '=='
	s[Kind.ne] = '!='
	s[Kind.gt] = '>'
	s[Kind.lt] = '<'
	s[Kind.ge] = '>='
	s[Kind.le] = '<='
	s[Kind.question] = '?'
	s[Kind.left_shift] = '<<'
	s[Kind.right_shift] = '>>'
	s[Kind.dollar] = '$'
	s[Kind.at] = '@'
	s[Kind.key_if] = 'if'
	s[Kind.key_else] = 'else'
	s[Kind.key_record] = 'record'
	s[Kind.key_return] = 'return'
	s[Kind.key_const] = 'const'
	s[Kind.key_mut] = 'mut'
	s[Kind.key_for] = 'for'
	s[Kind.key_fn] = 'fn'
	s[Kind.key_trait] = 'trait'
	s[Kind.key_true] = 'true'
	s[Kind.key_false] = 'false'
	s[Kind.key_continue] = 'continue'
	s[Kind.key_break] = 'break'
	s[Kind.key_unsafe] = 'unsafe'
	s[Kind.key_enum] = 'enum'
	s[Kind.key_pub] = 'pub'
	s[Kind.key_in] = 'in'
	s[Kind.key_union] = 'union'
	s[Kind.key_as] = 'as'
	s[Kind.key_defer] = 'defer'
	s[Kind.key_match] = 'match'
	s[Kind.key_none] = 'none'
	s[Kind.key_is] = 'is'
	// The following kinds are not for tokens returned by the Rivet tokenizer,
	// they are used just for organization/ease of checking:
	s[Kind.keyword_beg] = 'keyword_beg'
	s[Kind.keyword_end] = 'keyword_end'
	return s
}

@[inline]
pub fn is_key(key string) bool {
	return int(keywords[key]) > 0
}

@[inline]
pub fn (t Kind) is_assign() bool {
	return t in assign_tokens
}

@[inline]
pub fn (t Kind) str() string {
	idx := int(t)
	if idx < 0 || token_str.len <= idx {
		return 'unknown'
	}
	return token_str[idx]
}

@[inline]
pub fn (t Token) is_next_to(pre_token Token) bool {
	return t.pos.pos - pre_token.pos.pos == pre_token.pos.len
}

pub fn (t Token) str() string {
	mut s := t.kind.str()
	if s.len == 0 {
		eprintln('missing token kind string')
	} else if !s[0].is_letter() {
		// punctuation, operators
		return 'token `${s}`'
	}
	if is_key(t.lit) {
		s = 'keyword'
	}
	if t.lit != '' {
		// string contents etc
		s += ' `${t.lit}`'
	}
	return s
}
