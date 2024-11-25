// Copyright (C) 2024-present Jose Mendoza - All rights reserved. Use of this
// source code is governed by an MIT license that can be found in the LICENSE
// file.

module context

import term
import strings
import compiler.token

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

@[params]
struct ReportParams {
	pos              ?token.Pos
	msg              string
	type             ReportType
	do_report_source bool = true
	panic            bool
}

const backtick = `\``

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

	if params.pos != none {
		sb.write_string(term.bold(params.pos.str() + ':'))
		sb.write_u8(` `)
	}

	sb.write_string(term.bold(params.type.colorize()))
	sb.write_u8(` `)

	print_highlighted_message(params.msg, mut sb)
	if params.do_report_source {
		sb.write_u8(`\n`)
	}

	s := sb.str()
	if params.panic {
		panic(s)
	} else {
		eprintln(s)
	}
}

@[inline]
fn (mut r Report) ic_note(msg string) {
	r.print_message(msg: msg, type: .note, do_report_source: false)
}

@[inline]
fn (mut r Report) ic_warn(msg string) {
	r.print_message(msg: msg, type: .warn, do_report_source: false)
}

@[noreturn]
fn (mut r Report) ic_error(msg string) {
	r.print_message(msg: msg, type: .error, do_report_source: false)
	exit(101)
}

@[noreturn]
fn (mut r Report) ic_fatal(msg string) {
	r.print_message(msg: msg, type: .fatal, do_report_source: false, panic: true)
	for {}
}

fn (mut r Report) warn(msg string, pos token.Pos) {
	r.warnings++
	r.print_message(pos: pos, msg: msg, type: .warn)
}

fn (mut r Report) error(msg string, pos token.Pos) {
	r.errors++
	r.print_message(pos: pos, msg: msg, type: .error)
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
pub fn error(msg string, pos token.Pos) {
	mut r := get().report
	r.error(msg, pos)
}

@[inline]
pub fn warn(msg string, pos token.Pos) {
	mut r := get().report
	r.warn(msg, pos)
}

pub fn note(msg string) {}

pub fn help(msg string) {}
