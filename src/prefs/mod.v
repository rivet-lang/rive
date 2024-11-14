// Copyright (C) 2024-present Jose Mendoza - All rights reserved. Use of this
// source code is governed by an MIT license that can be found in the LICENSE
// file.

module prefs

import os
import flag
import util

@[footer: 'The compiler expects an input, either file or directory (if directory, it must contain a file entry `src/main.ri`).']
@[xdoc: 'The Rivet programming language compiler']
@[name: 'rivetc']
@[version: '0.1.0']
pub struct Prefs {
mut:
	input string @[ignore]

	show_help bool @[long: help; short: h; xdoc: 'Print help information.']
}

@[inline]
pub fn parse_args(args []string) !Prefs {
	mut prefs, remaining := flag.to_struct[Prefs](args)!

	if prefs.show_help {
		eprintln(flag.to_doc[Prefs]()!)
		exit(0)
	}

	if remaining.len == 1 {
		input := remaining[0]
		match true {
			os.is_file(input) {
				prefs.input = input
			}
			os.is_dir(input) {
				mut main_ri := os.join_path(input, 'main.ri')
				if os.exists(main_ri) {
					prefs.input = main_ri
				} else {
					main_ri = os.join_path(input, 'src', 'main.ri')
					if os.exists(main_ri) {
						prefs.input = main_ri
					} else {
						util.error("'${input}' is not a valid input, no file '${main_ri}' found")
					}
				}
			}
			else {
				util.error("'${input}' is not a valid input, expected file")
			}
		}
	} else if remaining.len == 0 {
		util.error('at least one input was expected')
	} else {
		util.error('only one input is expected')
	}

	return prefs
}
