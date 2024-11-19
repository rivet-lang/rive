// Copyright (C) 2024-present Jose Mendoza - All rights reserved. Use of this
// source code is governed by an MIT license that can be found in the LICENSE
// file.

module report

import term
import compiler.token

enum MsgLevel {
	note
	warn
	error
	ice
}

@[inline]
fn format_msg(msg string, level MsgLevel) string {
	return term.bold('rivetc: ') + match level {
		.note {
			'${term.bold(term.cyan('error:'))}'
		}
		.warn {
			'${term.bold(term.yellow('error:'))}'
		}
		.error {
			'${term.bold(term.red('error:'))}'
		}
		.ice {
			'${term.bold(term.red('internal compiler error:'))}'
		}
	} + ' ${msg}'
}

@[inline]
pub fn ic_note(msg string) {
	eprintln(format_msg(msg, .note))
}

@[inline]
pub fn ic_warn(msg string) {
	eprintln(format_msg(msg, .warn))
}

@[noreturn]
pub fn ic_error(msg string) {
	eprintln(format_msg(msg, .error))
	exit(101)
}

@[noreturn]
pub fn ic_fatal(msg string) {
	panic(format_msg(msg, .ice))
}

pub fn error(msg string, pos token.Pos) {}

pub fn warn(msg string, pos token.Pos) {}

pub fn note(msg string) {}

pub fn help(msg string) {}
