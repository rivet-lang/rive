// Copyright (C) 2024-present Jose Mendoza - All rights reserved. Use of this
// source code is governed by an MIT license that can be found in the LICENSE
// file.

module ast

import compiler.util

@[heap]
pub struct SourceFile {
pub:
	file    string
	content string
pub mut:
	lines ?[]string
}

pub fn SourceFile.new(file string) &SourceFile {
	content := util.read_file(file)
	return &SourceFile{
		file:    file
		content: content
	}
}

pub fn SourceFile.from_memory(content string) &SourceFile {
	return &SourceFile{
		file:    '<memory>'
		content: content
	}
}

pub fn (mut sf SourceFile) get_lines() []string {
	if sf.lines != none {
		return sf.lines
	}
	sf.lines = sf.content.split_into_lines()
	return sf.lines?
}
