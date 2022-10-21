# Copyright (C) 2022 The Rivet Team. All rights reserved.
# Use of this source code is governed by an MIT license
# that can be found in the LICENSE file.

from .token import Kind
from . import utils, token, report

LF = chr(10)
CR = chr(13)
NUM_SEP = "_"

def is_hex_digit(ch):
	return ch.isdigit() or (ch >= "a"
	                        and ch <= "f") or (ch >= "A" and ch <= "F")

def is_bin_digit(ch):
	return ch in ("0", "1")

class Conditional:
	def __init__(self):
		self.matched = False
		self.else_found = False
		self.skip_section = False

class Lexer:
	def __init__(self, comp, text):
		self.comp = comp
		self.file = "<in-memory>"
		self.text = text
		self.text_len = len(text)

		self.line = 0
		self.last_nl_pos = 0
		self.pos = 0

		self.is_started = False
		self.is_cr_lf = False

		self.conditional_stack = []

		self.all_tokens = []
		self.tidx = 0

	@staticmethod
	def from_file(comp, file):
		s = Lexer(comp, open(file, encoding = 'UTF-8').read())
		s.file = file
		s.tokenize_remaining_text()
		return s

	def tokenize_remaining_text(self):
		while True:
			t = self._next()
			self.all_tokens.append(t)
			if t.kind == Kind.EOF:
				break

	def current_char(self):
		return self.text[self.pos]

	def current_pos(self):
		return token.Pos(
		    self.file, self.line, max(1, self.current_column()), self.pos
		)

	def ignore_line(self):
		self.eat_to_end_of_line()
		self.inc_line_number()

	def eat_to_end_of_line(self):
		while self.pos < self.text_len and self.current_char() != LF:
			self.pos += 1

	def inc_line_number(self):
		self.last_nl_pos = min(self.text_len - 1, self.pos)
		if self.is_cr_lf:
			self.last_nl_pos += 1
		self.line += 1

	def current_column(self):
		return self.pos - self.last_nl_pos

	def is_new_line(self, ch):
		return ch in (CR, LF)

	def skip_whitespace(self):
		while self.pos < self.text_len:
			c = self.current_char()
			if c == chr(8):
				self.pos += 1
				continue
			if not (
			    c == chr(32) or (c > chr(8) and c < chr(14)) or
			    (c == chr(0x85)) or (c == chr(0xA0))
			):
				return
			if (
			    self.pos + 1 < self.text_len and c == CR
			    and self.text[self.pos + 1] == LF
			):
				self.is_cr_lf = True
			if self.is_new_line(c) and not (
			    self.pos > 0 and self.text[self.pos - 1] == CR and c == LF
			):
				self.inc_line_number()
			self.pos += 1

	def matches(self, want, start_pos):
		end_pos = start_pos + len(want)
		if (
		    start_pos < 0 or end_pos < 0 or start_pos >= self.text_len
		    or end_pos > self.text_len
		):
			return False
		for pos in range(start_pos, end_pos):
			if self.text[pos] != want[pos - start_pos]:
				return False
		return True

	def peek_token(self, n):
		idx = self.tidx + n
		if idx >= len(self.all_tokens):
			return token.Token("", Kind.EOF, self.current_pos())
		return self.all_tokens[idx]

	def look_ahead(self, pos):
		return self.text[self.pos +
		                 pos] if self.pos + pos < self.text_len else chr(0)

	def read_ident(self):
		start = self.pos
		self.pos += 1
		while self.pos < self.text_len:
			c = self.text[self.pos]
			if utils.is_valid_name(c) or c.isdigit():
				self.pos += 1
				continue
			break
		lit = self.text[start:self.pos]
		self.pos -= 1
		return lit

	def read_hex_number(self):
		start = self.pos
		self.pos += 2 # skip '0x'
		if self.pos < self.text_len and self.current_char() == NUM_SEP:
			report.error(
			    "separator `_` is only valid between digits in a numeric literal",
			    self.current_pos(),
			)
		while self.pos < self.text_len:
			ch = self.current_char()
			if ch == NUM_SEP and self.text[self.pos + 1] == NUM_SEP:
				report.error(
				    "cannot use `_` consecutively in a numeric literal",
				    self.current_pos()
				)
			if not is_hex_digit(ch) and ch != NUM_SEP:
				if not ch.isdigit() and not ch.isalpha():
					break
				else:
					report.error(
					    f"this hexadecimal number has unsuitable digit `{self.current_char()}`",
					    self.current_pos(),
					)
			self.pos += 1
		if self.text[self.pos - 1] == NUM_SEP:
			self.pos -= 1
			report.error(
			    "cannot use `_` at the end of a numeric literal",
			    self.current_pos()
			)
			self.pos += 1
		lit = self.text[start:self.pos]
		self.pos -= 1 # fix pos
		return lit

	def read_bin_number(self):
		start = self.pos
		has_wrong_digit = False
		self.pos += 2 # skip '0b'
		if self.pos < self.text_len and self.current_char() == NUM_SEP:
			report.error(
			    "separator `_` is only valid between digits in a numeric literal",
			    self.current_pos(),
			)
		while self.pos < self.text_len:
			ch = self.current_char()
			if ch == NUM_SEP and self.text[self.pos + 1] == NUM_SEP:
				report.error(
				    "cannot use `_` consecutively in a numeric literal",
				    self.current_pos()
				)
			if not is_bin_digit(ch) and ch != NUM_SEP:
				if not ch.isdigit() and not ch.isalpha():
					break
				elif not has_wrong_digit:
					has_wrong_digit = True
					report.error(
					    f"this binary number has unsuitable digit `{self.current_char()}`",
					    self.current_pos(),
					)
			self.pos += 1
		if self.text[self.pos - 1] == NUM_SEP:
			self.pos -= 1
			report.error(
			    "cannot use `_` at the end of a numeric literal",
			    self.current_pos()
			)
			self.pos += 1
		lit = self.text[start:self.pos]
		self.pos -= 1 # fix pos
		return lit

	def read_dec_number(self):
		start = self.pos
		while self.pos < self.text_len:
			ch = self.current_char()
			if ch == NUM_SEP and self.text[self.pos + 1] == NUM_SEP:
				report.error(
				    "cannot use `_` consecutively in a numeric literal",
				    self.current_pos()
				)
			if not ch.isdigit() and ch != NUM_SEP:
				if not ch.isalpha() or ch in ["e", "E"]:
					break
				else:
					report.error(
					    f"this number has unsuitable digit `{self.current_char()}`",
					    self.current_pos(),
					)
			self.pos += 1
		if self.text[self.pos - 1] == NUM_SEP:
			self.pos -= 1
			report.error(
			    "cannot use `_` at the end of a numeric literal",
			    self.current_pos()
			)
			self.pos += 1

		# fractional part
		if self.pos < self.text_len and self.text[self.pos] == ".":
			self.pos += 1
			if self.pos < self.text_len:
				# 16.6, 16.6.str()
				if self.text[self.pos].isdigit():
					while self.pos < self.text_len:
						c = self.text[self.pos]
						if not c.isdigit():
							if not c.isalpha() or c in ["e", "E"]:
								# 16.6.str()
								break
							else:
								report.error(
								    f"this number has unsuitable digit `{c}`",
								    self.current_pos(),
								)
						self.pos += 1
				elif self.text[self.pos] == ".":
					# 4.. a range
					self.pos -= 1
				elif self.text[self.pos] in ["e", "E"]:
					pass # 6.e6
				elif self.text[self.pos].isalpha():
					# 16.str()
					self.pos -= 1
				else:
					# 5.
					self.pos -= 1
					report.error(
					    "float literals should have a digit after the decimal point",
					    self.current_pos(),
					)
					fl = self.text[start:self.pos]
					report.help(f"use `{fl}.0` instead of `{fl}`")
					self.pos += 1

		# exponential part
		if self.pos < self.text_len and self.text[self.pos] in ["e", "E"]:
			self.pos += 1
			if self.pos < self.text_len and self.text[self.pos] in ["-", "+"]:
				self.pos += 1
			while self.pos < self.text_len:
				c = self.text[self.pos]
				if not c.isdigit():
					if not c.isalpha():
						# 6e6.str()
						break
					elif not has_suffix:
						report.error(
						    f"this number has unsuitable digit `{c}`",
						    self.current_pos()
						)
				self.pos += 1
		lit = self.text[start:self.pos]
		self.pos -= 1 # fix pos
		return lit

	def read_number(self):
		if self.matches("0x", self.pos):
			return self.read_hex_number()
		elif self.matches("0b", self.pos):
			return self.read_bin_number()
		return self.read_dec_number()

	def read_char(self):
		len_ = 0
		start = self.pos
		is_bytelit = self.pos > 0 and self.text[self.pos - 1] == "b"

		while True:
			self.pos += 1
			if self.pos >= self.text_len:
				break
			if self.current_char() != utils.BACKSLASH:
				len_ += 1
			double_slash = self.matches("\\\\", self.pos - 2)
			if self.current_char() == "'" and (
			    self.text[self.pos - 1] != utils.BACKSLASH or double_slash
			):
				if double_slash:
					len_ += 1
				break
		len_ -= 1

		ch = self.text[start + 1:self.pos]
		if len_ == 0:
			report.error("empty character literal", self.current_pos())
		elif is_bytelit:
			len_ = utils.bytestr(ch).len
			if len_ > 1 and ch != (utils.BACKSLASH * 2):
				report.error(
				    "byte literal may only contain one byte", self.current_pos()
				)
		elif len_ != 1:
			if len_ > 1:
				report.error(
				    "character literal may only contain one codepoint",
				    self.current_pos()
				)
				report.help(
				    "if you meant to write a string literal, use double quotes"
				)
		return ch

	def read_string(self):
		start_pos = self.current_pos()
		start = self.pos
		start_char = self.current_char()
		backslash_count = 1 if start_char == utils.BACKSLASH else 0
		is_raw = self.pos > 0 and self.text[self.pos - 1] == "r"
		is_cstr = self.pos > 0 and self.text[self.pos - 1] == "c"
		n_cr_chars = 0
		h_escapes_pos = [] #pos list of \xXX
		while True:
			self.pos += 1
			if self.pos >= self.text_len:
				self.pos = start
				report.error("unfinished string literal", start_pos)
				return ""
			c = self.current_char()
			if c == utils.BACKSLASH:
				backslash_count += 1
			# end of string
			if c == '"' and (is_raw or backslash_count % 2 == 0):
				break # handle "\\" at the end
			if c == CR:
				n_cr_chars += 1
			if c == LF:
				self.inc_line_number()
			if backslash_count % 2 == 1 and not (is_cstr or is_raw):
				# escape `\x`
				if c == "x":
					h_escapes_pos.append(self.pos - 1)
			if c != utils.BACKSLASH:
				backslash_count = 0
		lit = ""
		if start <= self.pos:
			lit = self.text[start + 1:self.pos]
			if len(h_escapes_pos) > 0:
				lit = utils.decode_h_escapes(lit, start + 1, h_escapes_pos)
			if n_cr_chars > 0:
				lit = lit.replace("\r", "")
		return lit

	def next(self):
		while True:
			cidx = self.tidx
			self.tidx += 1
			if cidx >= len(self.all_tokens):
				return token.Token("", Kind.EOF, self.current_pos())
			return self.all_tokens[cidx]
		return token.Token("", Kind.EOF, self.current_pos())

	def _next(self):
		while True:
			if self.is_started:
				self.pos += 1
			else:
				self.is_started = True
			self.skip_whitespace()
			if self.pos >= self.text_len:
				return token.Token("", Kind.EOF, self.current_pos())
			pos = self.current_pos()
			ch = self.current_char()
			nextc = self.look_ahead(1)
			if utils.is_valid_name(ch):
				lit = self.read_ident()
				return token.Token(lit, token.lookup(lit), pos)
			elif ch.isdigit():
				# decimals with 0 prefix
				start_pos = self.pos
				while start_pos < self.text_len and self.text[start_pos] == '0':
					start_pos += 1
				prefix_zero_num = start_pos - self.pos
				if start_pos == self.text_len or (
				    ch == '0' and not self.text[start_pos].isdigit()
				):
					prefix_zero_num -= 1
				self.pos += prefix_zero_num
				return token.Token(
				    self.read_number().replace("_", ""), Kind.Number, pos
				)

			# delimiters and operators
			if ch == "+":
				if nextc == "=":
					self.pos += 1
					return token.Token("", Kind.PlusAssign, pos)
				return token.Token("", Kind.Plus, pos)
			elif ch == "-":
				if nextc == "=":
					self.pos += 1
					return token.Token("", Kind.MinusAssign, pos)
				return token.Token("", Kind.Minus, pos)
			elif ch == "*":
				if nextc == "=":
					self.pos += 1
					return token.Token("", Kind.MulAssign, pos)
				return token.Token("", Kind.Mul, pos)
			elif ch == "/":
				if nextc == "/":
					start_pos = self.pos
					if self.matches("///", start_pos):
						start_pos += 3
						self.ignore_line()
						line = self.text[start_pos:self.pos].strip()
						return token.Token(line, Kind.DocComment, pos)
					self.ignore_line()
					continue
				elif nextc == "*":
					start_pos = self.pos
					self.pos += 1
					while self.pos < self.text_len - 1:
						self.pos += 1
						if self.current_char() == LF:
							self.inc_line_number()
							continue
						elif self.matches("*/", self.pos):
							self.pos += 1
							break
					if self.pos >= self.text_len:
						self.pos = start_pos
						report.error(
						    "comment not terminated", self.current_pos()
						)
					continue
				elif nextc == "=":
					self.pos += 1
					return token.Token("", Kind.DivAssign, pos)
				return token.Token("", Kind.Div, pos)
			elif ch == "%":
				if nextc == "=":
					self.pos += 1
					return token.Token("", Kind.ModAssign, pos)
				return token.Token("", Kind.Mod, pos)
			#
			elif ch == "=":
				if nextc == "=":
					self.pos += 1
					return token.Token("", Kind.Eq, pos)
				elif nextc == ">":
					self.pos += 1
					return token.Token("", Kind.Arrow, pos)
				return token.Token("", Kind.Assign, pos)
			#
			elif ch == "<":
				if nextc == "=":
					self.pos += 1
					return token.Token("", Kind.Le, pos)
				return token.Token("", Kind.Lt, pos)
			elif ch == ">":
				if nextc == "=":
					self.pos += 1
					return token.Token("", Kind.Ge, pos)
				return token.Token("", Kind.Gt, pos)
			#
			elif ch == ".":
				if nextc == "." and self.text[self.pos + 2] == ".":
					self.pos += 2
					return token.Token("", Kind.Ellipsis, pos)
				elif nextc == ".":
					self.pos += 1
					return token.Token("", Kind.DotDot, pos)
				return token.Token("", Kind.Dot, pos)
			elif ch == ",":
				return token.Token("", Kind.Comma, pos)
			elif ch == ":":
				if nextc == ":":
					self.pos += 1
					return token.Token("", Kind.DoubleColon, pos)
				return token.Token("", Kind.Colon, pos)
			elif ch == ";":
				return token.Token("", Kind.Semicolon, pos)
			elif ch == "?":
				if nextc == "?":
					self.pos += 1
					return token.Token("", Kind.OrElse, pos)
				return token.Token("", Kind.Question, pos)
			elif ch == "$":
				return token.Token("", Kind.Dollar, pos)
			elif ch == "#":
				if nextc != "[":
					self.pp_directive()
					continue
				return token.Token("", Kind.Hash, pos)
			elif ch == "&":
				if nextc == "=":
					self.pos += 1
					return token.Token("", Kind.AmpAssign, pos)
				return token.Token("", Kind.Amp, pos)
			elif ch == "!":
				if (
				    self.matches("is", self.pos)
				    and self.text[self.pos + 3].isspace()
				):
					self.pos += 2
					return token.Token("", Kind.KeyNotIs, pos)
				elif (
				    self.matches("in", self.pos)
				    and self.text[self.pos + 3].isspace()
				):
					self.pos += 2
					return token.Token("", Kind.KeyNotIn, pos)
				elif nextc == "=":
					self.pos += 1
					return token.Token("", Kind.Ne, pos)
				return token.Token("", Kind.Bang, pos)
			elif ch == "|":
				if nextc == "=":
					self.pos += 1
					return token.Token("", Kind.PipeAssign, pos)
				return token.Token("", Kind.Pipe, pos)
			elif ch == "~":
				return token.Token("", Kind.BitNot, pos)
			elif ch == "^":
				if nextc == "=":
					self.pos += 1
					return token.Token("", Kind.XorAssign, pos)
				return token.Token("", Kind.Xor, pos)
			#
			elif ch == "{":
				return token.Token("", Kind.Lbrace, pos)
			elif ch == "}":
				return token.Token("", Kind.Rbrace, pos)
			elif ch == "[":
				return token.Token("", Kind.Lbracket, pos)
			elif ch == "]":
				return token.Token("", Kind.Rbracket, pos)
			elif ch == "(":
				return token.Token("", Kind.Lparen, pos)
			elif ch == ")":
				return token.Token("", Kind.Rparen, pos)
			# characters and strings
			elif ch == "'":
				return token.Token(self.read_char(), Kind.Char, pos)
			elif ch == '"':
				return token.Token(self.read_string(), Kind.String, pos)
			else:
				report.error(f"invalid character `{ch}`", pos)
				break
		return token.Token("", Kind.EOF, self.current_pos())

	def pp_directive(self):
		pos = self.current_pos()
		self.pos += 1 # skip '#'
		self.skip_whitespace()
		kw = self.read_ident()

		if kw == "if":
			self.pos += 1 #fix
			self.pp_if()
		elif kw == "elif":
			self.pos += 1 #fix
			self.pp_elif()
		elif kw == "else":
			self.pp_else()
		elif kw == "endif":
			self.pp_endif()
		else:
			report.error(f"invalid preprocessing directive: `{kw}`", pos)
			return

		if len(self.conditional_stack
		       ) > 0 and self.conditional_stack[-1].skip_section:
			# skip tokens until next preprocessing directive
			while self.pos < self.text_len:
				cc = self.current_char()
				if cc == '#':
					self.pos -= 1
					return
				if cc == '\n':
					self.inc_line_number()
				self.pos += 1
			# if we get EOF, then no corresponding `#endif` has been written
			if self.pos == self.text_len:
				report.error("expected `#endif`, found end of file", pos)

	def pp_if(self):
		self.skip_whitespace()
		cond = self.pp_expression()
		self.conditional_stack.append(Conditional())
		if cond and (
		    len(self.conditional_stack) == 1
		    or not self.conditional_stack[-2].skip_section
		):
			# condition true => process code within if
			self.conditional_stack[-1].matched = True
		else:
			# skip lines until next preprocessing directive
			self.conditional_stack[-1].skip_section = True

	def pp_elif(self):
		pos = self.current_pos()
		self.skip_whitespace()
		cond = self.pp_expression()
		if len(self.conditional_stack
		       ) == 0 or self.conditional_stack[-1].else_found:
			report.error("unexpected `#elif`", pos)
			return

		if cond and (not self.conditional_stack[-1].matched) and (
		    len(self.conditional_stack) == 1
		    or not self.conditional_stack[-2].skip_section
		):
			# condition true => process code within if
			self.conditional_stack[-1].matched = True
			self.conditional_stack[-1].skip_section = False
		else:
			# skip lines until next preprocessing directive
			self.conditional_stack[-1].skip_section = True

	def pp_else(self):
		pos = self.current_pos()
		self.skip_whitespace()
		if len(self.conditional_stack
		       ) == 0 or self.conditional_stack[-1].else_found:
			report.error("unexpected `#else`", pos)
			return

		if (not self.conditional_stack[-1].matched) and (
		    len(self.conditional_stack) == 1
		    or not self.conditional_stack[-2].skip_section
		):
			# condition true => process code within if
			self.conditional_stack[-1].matched = True
			self.conditional_stack[-1].skip_section = False
		else:
			# skip lines until next preprocessing directive
			self.conditional_stack[-1].skip_section = True

	def pp_endif(self):
		pos = self.current_pos()
		if len(self.conditional_stack) == 0:
			report.error("unexpected `#endif`", pos)
			return
		self.conditional_stack.pop()

	def pp_expression(self):
		return self.pp_or_expression()

	def pp_or_expression(self):
		left = self.pp_and_expression()
		self.skip_whitespace()
		while self.pos < self.text_len and self.matches("or", self.pos):
			self.pos += 2
			self.skip_whitespace()
			right = self.pp_and_expression()
			left = left or right
		return left

	def pp_and_expression(self):
		left = self.pp_equality_expression()
		self.skip_whitespace()
		while self.pos < self.text_len and self.matches("and", self.pos):
			self.pos += 3
			self.skip_whitespace()
			right = self.pp_equality_expression()
			left = left and right
		return left

	def pp_equality_expression(self):
		left = self.pp_unary_expression()
		self.skip_whitespace()
		while True:
			if self.pos < self.text_len and self.matches("==", self.pos):
				self.pos += 2
				self.skip_whitespace()
				right = self.pp_unary_expression()
				left = left == right
			elif self.pos < self.text_len and self.matches("!=", self.pos):
				self.pos += 2
				self.skip_whitespace()
				right = self.pp_unary_expression()
				left = left != right
			else:
				break
		return left

	def pp_unary_expression(self):
		if self.pos < self.text_len and self.matches("!", self.pos):
			self.pos += 1
			self.skip_whitespace()
			return not self.pp_unary_expression()
		return self.pp_primary_expression()

	def pp_primary_expression(self):
		pos = self.current_pos()
		cc = self.current_char()
		if self.pos >= self.text_len:
			report.error("expected identifier", pos)
		elif utils.is_valid_name(cc):
			return self.pp_symbol()
		elif cc == '(':
			self.pos += 1
			self.skip_whitespace()
			result = self.pp_expression()
			self.skip_whitespace()
			if self.pos < self.text_len and self.current_char() == ')':
				self.pos += 1
			else:
				report.error("expected `)`", self.current_pos())
			return result
		return False

	def pp_symbol(self):
		pos = self.current_pos()
		ident = self.read_ident()
		defined = False
		if ident == "true":
			defined = True
		elif ident == "false":
			defined = False
		else:
			defined = self.comp.evalue_pp_symbol(ident, pos)
		return defined
