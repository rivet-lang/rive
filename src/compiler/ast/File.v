// Copyright (C) 2024-present Jose Mendoza - All rights reserved. Use of this
// source code is governed by an MIT license that can be found in the LICENSE
// file.

module ast

import os

@[heap]
pub struct File {
pub:
	filename string
	content  string
mut:
	lines ?[]string
}

pub struct FilePos {
pub mut:
	file  &File = unsafe { nil }
	begin FileLoc
	end   FileLoc
}

pub fn (fp &FilePos) contains(loc &FileLoc) bool {
	if loc.line > fp.begin.line && loc.line < fp.end.line {
		return true
	} else if loc.line == fp.begin.line && loc.line == fp.end.line {
		return loc.col >= fp.begin.col && loc.col <= fp.end.col
	} else if loc.line == fp.begin.line {
		return loc.col >= fp.begin.col
	} else if loc.line == fp.end.line {
		return loc.col <= fp.end.col
	}
	return false
}

pub fn (fp &FilePos) str() string {
	if fp.begin.line == fp.end.line {
		return '${fp.file.filename}:${fp.begin.line + 1}:${fp.begin.col}'
	}
	return '${fp.file.filename}:${fp.begin.line + 1}:${fp.begin.col}-${fp.end.line + 1}:${fp.end.col}'
}

pub struct FileLoc {
pub:
	pos  int
	line int
	col  int
}

pub fn File.new(file string) &File {
	content := read_file(file)
	return &File{
		filename: file
		content:  content
	}
}

pub fn File.from_memory(content string) &File {
	return &File{
		filename: '<memory>'
		content:  content
	}
}

pub fn (file &File) get_lines() []string {
	if file.lines != none {
		return file.lines
	}
	lines := file.content.split_into_lines()
	unsafe {
		file.lines = lines
	}
	return lines
}

pub fn (file &File) get_line(line int) ?string {
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
