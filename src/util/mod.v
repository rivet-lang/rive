// Copyright (C) 2024-present Jose Mendoza - All rights reserved. Use of this
// source code is governed by an MIT license that can be found in the LICENSE
// file.

module util

import term
import os

enum MsgLevel {
	note
	warn
	error
	ice
}

@[inline]
fn format_msg(msg string, level MsgLevel) string {
	return match level {
		.note {
			'${term.bold('rivetc: ')}${term.bold(term.cyan('error:'))} ${msg}'
		}
		.warn {
			'${term.bold('rivetc: ')}${term.bold(term.yellow('error:'))} ${msg}'
		}
		.error {
			'${term.bold('rivetc: ')}${term.bold(term.red('error:'))} ${msg}'
		}
		.ice {
			'${term.bold('rivetc: ')}${term.bold(term.red('internal compiler error:'))} ${msg}'
		}
	}
}

pub fn note(msg string) {
	eprintln(format_msg(msg, .note))
}

pub fn warn(msg string) {
	eprintln(format_msg(msg, .warn))
}

@[noreturn]
pub fn error(msg string) {
	eprintln(format_msg(msg, .error))
	exit(101)
}

@[noreturn]
pub fn ic_error(msg string) {
	panic(format_msg(msg, .ice))
}

pub fn read_file(path string) string {
	return skip_bom(os.read_file(path) or {
		// we use `ic_error` because this should not happen
		ic_error(err.msg())
		''
	})
}

pub fn skip_bom(file_content string) string {
	mut raw_text := file_content
	// BOM check
	if raw_text.len >= 3 {
		unsafe {
			c_text := raw_text.str
			if c_text[0] == 0xEF && c_text[1] == 0xBB && c_text[2] == 0xBF {
				// skip three BOM bytes
				offset_from_begin := 3
				raw_text = tos(c_text[offset_from_begin], vstrlen(c_text) - offset_from_begin)
			}
		}
	}
	return raw_text
}
