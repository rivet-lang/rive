// Copyright (C) 2024-present Jose Mendoza - All rights reserved. Use of this
// source code is governed by an MIT license that can be found in the LICENSE
// file.

module compiler

import compiler.prefs
import compiler.report

pub struct Compiler {
pub mut:
	prefs &prefs.Prefs
}

pub fn run(args []string) {
	mut c := Compiler{
		prefs: prefs.parse_args(args) or { report.error(err.msg()) }
	}
}
