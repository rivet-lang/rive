// Copyright (C) 2024-present Jose Mendoza - All rights reserved. Use of this
// source code is governed by an MIT license that can be found in the LICENSE
// file.

module ast

pub type Type = SimpleType | PointerType

pub struct SimpleType {
pub:
	expr Expr
	sym  ?Symbol
	pos  FilePos
}

pub struct PointerType {
pub:
	inner Type
	pos   FilePos
}
