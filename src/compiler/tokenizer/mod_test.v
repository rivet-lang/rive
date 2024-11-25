// Copyright (C) 2024-present Jose Mendoza - All rights reserved. Use of this
// source code is governed by an MIT license that can be found in the LICENSE
// file.
import compiler.token
import compiler.context
import compiler.tokenizer

struct ExpectedToken {
	kind token.Kind
	lit  string
}

const source = 'fn main 123 123.0 0b0101 0o12345678 0x123456ABCDEF'
const expected_tokens = [
	ExpectedToken{.kw_fn, 'fn'},
	ExpectedToken{.ident, 'main'},
	ExpectedToken{.number, '123'},
	ExpectedToken{.number, '123.0'},
	ExpectedToken{.number, '0b0101'},
	ExpectedToken{.number, '0o12345678'},
	ExpectedToken{.number, '0x123456ABCDEF'},
	ExpectedToken{.eof, ''},
]

fn test_tokenizer_next() {
	mut c_ctx := &context.CContext{
		options: unsafe { nil }
	}
	mut t := tokenizer.from_memory(c_ctx, source)
	mut tokens := []token.Token{}
	for tok := t.next(); tok.kind != .eof; tok = t.next() {
		tokens << tok
	}
	for i, tok in tokens {
		expected_tok := expected_tokens[i]
		assert expected_tok.kind == tok.kind
		assert expected_tok.lit == tok.lit
	}
}
