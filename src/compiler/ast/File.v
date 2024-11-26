// Copyright (C) 2024-present Jose Mendoza - All rights reserved. Use of this
// source code is governed by an MIT license that can be found in the LICENSE
// file.

module ast

import os

@[heap]
pub struct File {
pub:
	file    string
	content string
pub mut:
	lines ?[]string
}

pub fn File.new(file string) &File {
	content := read_file(file)
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

pub fn (mut file File) get_lines() []string {
	if file.lines != none {
		return file.lines
	}
	lines := file.content.split_into_lines()
	file.lines = lines
	return lines
}

pub fn (mut file File) get_line(line int) ?string {
	lines := file.get_lines()
	return lines[line] or { none }
}

pub fn read_file(path string) string {
	return skip_bom(os.read_file(path) or {
		// we use `panic` because this should not happen
		panic(err.msg())
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
