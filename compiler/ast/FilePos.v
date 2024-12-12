// Copyright (C) 2024-present The Rivet programming language. Use of this source code
// is governed by an MIT license that can be found in the LICENSE file.

module ast

@[minify]
pub struct FileLoc {
pub:
	pos  int
	line int
	col  int
}

@[minify]
pub struct FilePos {
pub mut:
	file  &File = unsafe { nil }
	begin FileLoc
	end   FileLoc
}

pub fn (fp FilePos) == (fp2 FilePos) bool {
	if !(fp.begin == fp2.begin && fp.end == fp2.end) {
		return false
	}
	return fp.file == fp2.file
}

@[inline]
pub fn (fp &FilePos) extend(end &FilePos) FilePos {
	return FilePos{
		...fp
		end: end.end
	}
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
	filename := if fp.file == unsafe { nil } { '<unknown-file>' } else { fp.file.filename }
	if fp.begin.line == fp.end.line {
		return '${filename}:${fp.begin.line + 1}:${fp.begin.col}'
	}
	return '${filename}:${fp.begin.line + 1}:${fp.begin.col}-${fp.end.line + 1}:${fp.end.col}'
}
