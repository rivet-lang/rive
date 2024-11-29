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

	files []&ast.File
}

pub fn (ctx CContext) get_file(filename string) ?&ast.File {
	for file in ctx.files {
		if file.filename == filename {
			return file
		}
	}
	return none
}
