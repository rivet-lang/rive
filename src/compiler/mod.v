// Copyright (C) 2024-present Jose Mendoza - All rights reserved. Use of this
// source code is governed by an MIT license that can be found in the LICENSE
// file.

module compiler

import compiler.context
import compiler.report

import compiler.tokenizer as _

pub fn run(args []string) {
	mut c_ctx := &context.CContext{
		options: context.parse_args(args) or { report.error(err.msg()) }
	}
}
