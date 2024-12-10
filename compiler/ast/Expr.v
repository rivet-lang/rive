// Copyright (C) 2024-present The Rivet programming language. Use of this source code
// is governed by an MIT license that can be found in the LICENSE file.

module ast

pub type Expr = EmptyExpr
	| Ident
	| CharLiteral
	| IntegerLiteral
	| FloatLiteral
	| StringLiteral
	| UnaryExpr
	| BinaryExpr

pub struct EmptyExpr {
pub:
	pos FilePos
}

pub const empty_expr = Expr(EmptyExpr{})

pub struct Ident {
pub:
	name  string
	scope &Scope
	pos   FilePos
}

pub struct IntegerLiteral {
pub:
	value string
	pos   FilePos
}

pub struct FloatLiteral {
pub:
	value string
	pos   FilePos
}

pub struct CharLiteral {
pub:
	value   string
	is_byte bool
	pos     FilePos
}

pub enum StringType {
	normal
	c_string
	bytes
	raw_string
}

pub struct StringLiteral {
pub:
	value        string
	literal_type StringType
	pos          FilePos
}

pub enum UnaryOp {
	unknown
	amp     // &
	bang    // !
	bit_not // ~
	minus   // -
}

pub struct UnaryExpr {
pub:
	right Expr
	op    UnaryOp
	pos   FilePos
}

pub enum BinaryOp {
	unknown
	plus    // +
	minus   // -
	mul     // *
	div     // /
	mod     // %
	xor     // ^
	pipe    // |
	amp     // &
	log_and // &&
	log_or  // ||
	lshift  // <<
	rshift  // >>
	not_in  // !in
	not_is  // !is
	eq      // ==
	ne      // !=
	gt      // >
	lt      // <
	ge      // >=
	le      // <=
	or_else // ??
	kw_in   // !in
	kw_is   // !is
}

pub struct BinaryExpr {
pub:
	left  Expr
	op    BinaryOp
	right Expr
	pos   FilePos
}
