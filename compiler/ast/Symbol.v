// Copyright (C) 2024-present The Rivet programming language. Use of this source code
// is governed by an MIT license that can be found in the LICENSE file.

module ast

pub type Symbol = Function | Constant | TypeSym

pub struct TypeSym {
pub:
	name   string
	kind   TypeKind
	fields []Field
}

pub enum TypeKind {
	unknown
	alias

	i8
	i16
	i32
	i64
	int

	u8
	u16
	u32
	u64
	uint

	f32
	f64

	bool
	rune

	array
	slice
	tuple
	struct
	trait
	enum
}

pub struct Field {
pub:
	name string
	type Type
}

pub struct Function {
pub:
	name string
	args []FnArg
	node FnStmt
}

pub struct Constant {
pub:
	name string
}

pub struct Variable {
pub:
	name     string
	is_local bool
}
