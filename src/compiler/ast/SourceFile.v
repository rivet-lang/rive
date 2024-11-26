// Copyright (C) 2024-present Jose Mendoza - All rights reserved. Use of this
// source code is governed by an MIT license that can be found in the LICENSE
// file.

module ast

import compiler.util

@[heap]
pub struct File {
pub:
	file    string
	content string
pub mut:
	lines ?[]string
}

pub fn File.new(file string) &File {
	content := util.read_file(file)
	return &File{
		file:    file
		content: content
	}
}

pub fn File.from_memory(content string) &File {
	return &File{
		file:    '<memory>'
		content: content
	}
}

pub fn (mut sf File) get_lines() []string {
	if sf.lines != none {
		return sf.lines
	}
	sf.lines = sf.content.split_into_lines()
	return sf.lines?
}
