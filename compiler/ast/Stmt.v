// Copyright (C) 2024-present The Rivet programming language. Use of this source code
// is governed by an MIT license that can be found in the LICENSE file.

module ast

pub type Stmt = EmptyStmt | FnStmt | ExprStmt | LetStmt | WhileStmt

pub type EmptyStmt = u8

pub const empty_stmt = Stmt(EmptyStmt(0))

pub struct ExprStmt {
pub:
	expr Expr
}

pub struct LetStmt {
pub:
	lefts  []Variable
	right  Expr
	is_pub bool
}

pub struct FnStmt {
pub:
	is_pub      bool
	name        string
	name_pos    FilePos
	args        []FnArg
	return_type Type
	stmts       []Stmt
}

pub struct FnArg {
pub:
	name         string
	name_pos     FilePos
	type         Type
	default_expr ?Expr
}

pub struct WhileStmt {
pub:
	init_stmt     ?Stmt
	cond          Expr
	continue_expr ?Expr
	stmts         []Stmt
}
