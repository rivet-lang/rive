// Copyright (C) 2024-present The Rivet programming language. Use of this source code
// is governed by an MIT license that can be found in the LICENSE file.
import os
import term

if !os.exists('rivetc') {
	panic('rivetc executable not found')
}

files := os.walk_ext('tests/', '.ri')
if files.len == 0 {
	return
}
for file in files {
	if !file.ends_with('.err.ri') {
		continue
	}
	println(term.bold('>> generating .out file for `${file}`'))
	res := os.execute('./rivetc ${file}')
	if res.exit_code != 0 {
		os.write_file(file#[..-3] + '.out', res.output.trim_space())!
	} else {
		println('   >> unexpected .exit_code == 0 for `${file}`')
	}
}
