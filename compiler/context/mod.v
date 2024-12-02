// Copyright (C) 2024-present Jose Mendoza - All rights reserved. Use of this
// source code is governed by an MIT license that can be found in the LICENSE
// file.

module context

import ast

const stack = []&CContext{}

pub fn push(ctx &CContext) {
	unsafe {
		stack << ctx
	}
}

pub fn get() &CContext {
	if stack == [] {
		panic('empty ccontext stack')
	}
	return stack.last()
}

pub fn pop() {
	if stack == [] {
		panic('empty ccontext stack')
	}
	unsafe {
		_ = stack.pop()
	}
}

@[heap]
pub struct CContext {
pub mut:
	options Options
	report  Report

	root_file &ast.File = unsafe { nil }
	files     []&ast.File
}

pub fn (ctx &CContext) abort_if_errors() {
	if ctx.report.errors > 0 {
		reason := if ctx.report.errors == 1 {
			'aborting due to previous error'
		} else {
			'aborting due to ${ctx.report.errors} previous errors'
		}
		ic_error('could not compile `${ctx.root_file.mod_name}` module, ${reason}')
	}
}
