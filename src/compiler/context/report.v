// Copyright (C) 2024-present Jose Mendoza - All rights reserved. Use of this
// source code is governed by an MIT license that can be found in the LICENSE
// file.

module context

import term
import strings
import compiler.ast

pub struct Report {
mut:
	errors   int
	warnings int
}

enum ReportType {
	fatal
	error
	warn
	note
}

fn (rt ReportType) colorize() string {
	return match rt {
		.fatal { term.red('fatal:') }
		.error { term.red('error:') }
		.warn { term.yellow('warning:') }
		.note { term.cyan('note:') }
	}
}

pub enum HintKind {
	note
	help
}

fn (hk HintKind) colorize() string {
	return term.bold(match hk {
		.note { term.cyan('note:') }
		.help { term.green('help:') }
	})
}

pub struct Hint {
pub:
	kind HintKind
	msg  string
	pos  ?ast.FilePos
}

@[params]
struct ReportParams {
	pos   ?ast.FilePos
	msg   string
	type  ReportType
	hints []Hint
}

const backtick = `\``
const border = term.bold(term.blue('      | '))

fn report_source(pos ast.FilePos, prefix string, mut sb strings.Builder) {
	for idx := pos.begin.line; idx <= pos.end.line; idx++ {
		if offending_line := pos.file.get_line(idx) {
			sb.writeln(border)
			sb.write_string(term.bold(term.blue('${pos.begin.line + 1:5d} | ')))
			sb.writeln(offending_line)
			sb.write_string(border)
			for jdx in 0 .. offending_line.len {
				if offending_line[jdx] == `\t` {
					sb.write_u8(`\t`)
					continue
				}
				/*
				println("${jdx} == ${pos.begin.col}")
				if jdx == pos.begin.col-1 {
					sb.write_string(term.green(term.bold('^')))
				} else if jdx > pos.begin.col-1 && jdx < pos.end.col-1 {
					sb.write_string(term.green(term.bold('~')))
				} else {
					sb.write_string(' ')
				}
				
				
				bool caret = false;
				unowned SourceLocation begin = source.begin;
				unowned SourceLocation end = source.end;
				if (begin.line == idx && end.line == idx) {
					if (begin.column <= jdx + 1 <= end.column) {
						caret = true;
					}
				} else if (begin.line == idx && begin.column <= jdx + 1) {
					caret = true;
				} else if (begin.line < idx < end.line) {
					caret = true;
				} else if (end.line == idx && end.column >= jdx + 1) {
					caret = true;
				}
				if (caret) {
					if (begin.line == idx && begin.column == jdx + 1) {
						stderr.putc ('^');
					} else {
						stderr.putc ('~');
					}
				} else {
					stderr.putc (' ');
				}*/
				mut caret := false
				if pos.begin.line == idx && pos.end.line == idx {
					if pos.begin.col <= jdx + 1 && jdx + 1 <= pos.end.col {
						caret = true
					}
				} else if pos.begin.line == idx && pos.begin.col <= jdx + 1 {
					caret = true
				} else if pos.begin.line < idx && idx < pos.end.line {
					caret = true
				} else if pos.end.line == idx && pos.end.col >= jdx + 1 {
					caret = true
				}
				if caret {
					if pos.begin.line == idx && pos.begin.col == jdx + 1 {
						sb.write_string(term.green(term.bold('^')))
					} else {
						sb.write_string(term.green(term.bold('~')))
					}
				} else {
					sb.write_u8(` `)
				}
			}
			sb.write_u8(`\n`)
		}
	}
}

@[direct_array_access]
fn print_highlighted_message(msg string, mut sb strings.Builder) {
	mut start := 0
	mut cur := 0

	for cur < msg.len {
		if msg[cur] == backtick {
			sb.write_string(msg[start..cur])
			start = cur
			cur++

			for cur < msg.len && msg[cur] != backtick {
				cur++
			}

			if cur == msg.len {
				sb.write_string(msg[start..cur])
				start = cur
			} else {
				cur++
				sb.write_string(term.cyan(msg[start..cur]))
				start = cur
			}
		} else {
			cur++
		}
	}

	sb.write_string(msg[start..])
}

fn (r Report) print_message(params ReportParams) {
	mut sb := strings.new_builder(params.msg.len)

	// file:line:column
	if params.pos != none {
		sb.write_string(term.bold(params.pos.str() + ':'))
		sb.write_u8(` `)
	}

	// error:|warn:|note:|help:
	sb.write_string(term.bold(params.type.colorize()))
	sb.write_u8(` `)

	// invalid character `\`
	print_highlighted_message(params.msg, mut sb)

	// 8 | '\'
	if params.pos != none {
		sb.write_u8(`\n`)
		report_source(params.pos, '', mut sb)
	}

	// help: remove invalid character
	for hint in params.hints {
		eq := term.bold(term.blue('='))
		sb.writeln('${' ':6}${eq} ${hint.kind.colorize()} ${hint.msg}')
		if hint.pos != none {
			// report_source()
		}
	}

	s := sb.str()
	if params.type == .fatal {
		panic(s)
	} else {
		eprint(s)
	}
}

@[inline]
fn (mut r Report) ic_note(msg string) {
	r.print_message(msg: msg, type: .note)
}

@[inline]
fn (mut r Report) ic_warn(msg string) {
	r.print_message(msg: msg, type: .warn)
}

@[noreturn]
fn (mut r Report) ic_error(msg string) {
	r.print_message(msg: msg, type: .error)
	exit(101)
}

@[noreturn]
fn (mut r Report) ic_fatal(msg string) {
	r.print_message(msg: msg, type: .fatal)
	for {}
}

fn (mut r Report) warn(msg string, pos ast.FilePos, hints ...Hint) {
	r.warnings++
	r.print_message(pos: pos, msg: msg, type: .warn, hints: hints)
}

fn (mut r Report) error(msg string, pos ast.FilePos, hints ...Hint) {
	r.errors++
	r.print_message(pos: pos, msg: msg, type: .error, hints: hints)
}

// convenience methods calling on correct instance:

@[inline]
pub fn ic_note(msg string) {
	mut r := get().report
	r.ic_note(msg)
}

@[inline]
pub fn ic_warn(msg string) {
	mut r := get().report
	r.ic_warn(msg)
}

@[noreturn]
pub fn ic_error(msg string) {
	mut r := get().report
	r.ic_error(msg)
}

@[noreturn]
pub fn ic_fatal(msg string) {
	mut r := get().report
	r.ic_fatal(msg)
}

@[inline]
pub fn error(msg string, pos ast.FilePos, hints ...Hint) {
	mut r := get().report
	r.error(msg, pos, ...hints)
}

@[inline]
pub fn warn(msg string, pos ast.FilePos, hints ...Hint) {
	mut r := get().report
	r.warn(msg, pos, ...hints)
}
