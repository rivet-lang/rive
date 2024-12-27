// Copyright (C) 2024-present The Rivet programming language. Use of this source code
// is governed by an MIT license that can be found in the LICENSE file.

module ast

pub type Stmt = EmptyStmt | FnStmt | ExprStmt | LetStmt | WhileStmt | DeferStmt

pub type EmptyStmt = u8

pub const empty_stmt = Stmt(EmptyStmt(0))

pub struct ExprStmt {
pub:
	tags Tags
	expr Expr
}

pub struct LetStmt {
pub:
	tags   Tags
	lefts  []Variable
	right  Expr
	is_pub bool
}

pub struct FnStmt {
pub:
	tags        Tags
	is_pub      bool
	name        string
	name_pos    FilePos
	args        []FnArg
	return_type Type
pub mut:
	stmts []Stmt
	sym   &Function = unsafe { nil }
	scope &Scope    = unsafe { nil }
}

pub struct FnArg {
pub:
	name         string
	name_pos     FilePos
	type         Type
	default_expr ?Expr
	pos          FilePos
}

pub struct WhileStmt {
pub:
	tags          Tags
	init_stmt     ?LetStmt
	cond          Expr
	continue_expr ?Expr
	stmts         []Stmt
}

pub enum DeferMode {
	default
	success
	error
}

pub struct DeferStmt {
pub:
	tags  Tags
	mode  DeferMode
	stmts []Stmt
}
