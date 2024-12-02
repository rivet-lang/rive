// Copyright (C) 2024-present Jose Mendoza - All rights reserved. Use of this
// source code is governed by an MIT license that can be found in the LICENSE
// file.

module ast

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
