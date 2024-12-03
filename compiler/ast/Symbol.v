// Copyright (C) 2024-present Jose Mendoza - All rights reserved. Use of this
// source code is governed by an MIT license that can be found in the LICENSE
// file.

module ast

pub type Symbol = Function | Constant

pub struct Function {
pub:
	name string
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
