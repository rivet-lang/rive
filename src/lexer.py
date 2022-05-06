# Copyright (C) 2022 The Rivet Team. All rights reserved.
# Use of this source code is governed by an MIT license
# that can be found in the LICENSE file.

from .tokens import Kind
from . import utils, tokens, report

LF = chr(10)
CR = chr(13)
NUM_SEP = "_"

def is_hex_number(ch):
    return ch.isdigit() or (ch >= "a"
                            and ch <= "f") or (ch >= "A" and ch <= "F")

def is_bin_number(ch):
    return ch in ("0", "1")

class Lexer:
    def __init__(self, text):
        self.file = "<in-memory>"
        self.text = text
        self.text_len = len(text)
        self.line = 0
        self.last_nl_pos = 0
        self.pos = 0
        self.is_started = False
        self.is_cr_lf = False

        self.all_tokens = []
        self.tidx = 0

    @staticmethod
    def from_file(file):
        s = Lexer(open(file).read())
        s.file = file
        s.tokenize_remaining_text()
        return s

    def tokenize_remaining_text(self):
        while True:
            t = self._next()
            self.all_tokens.append(t)
            if t.kind == Kind.EOF:
                break

    def cur_char(self):
        return self.text[self.pos]

    def get_pos(self):
        return tokens.Position(
            self.file, self.line, max(1, self.current_column()), self.pos
        )

    def ignore_line(self):
        self.eat_to_end_of_line()
        self.inc_line_number()

    def eat_to_end_of_line(self):
        while self.pos < self.text_len and self.cur_char() != LF:
            self.pos += 1

    def inc_line_number(self):
        self.last_nl_pos = min(self.text_len - 1, self.pos)
        if self.is_cr_lf:
            self.last_nl_pos += 1
        self.line += 1

    def current_column(self):
        return self.pos - self.last_nl_pos

    def is_nl(self, ch):
        return ch in (CR, LF)

    def skip_whitespace(self):
        while self.pos < self.text_len:
            c = self.cur_char()
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
            if self.is_nl(c) and not (
                self.pos > 0 and self.text[self.pos - 1] == CR and c == LF
            ):
                self.inc_line_number()
            self.pos += 1

    def expect(self, want, start_pos):
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
            return tokens.Token("", Kind.EOF, self.get_pos())
        return self.all_tokens[idx]

    def read_ident(self):
        start = self.pos
        self.pos += 1
        while self.pos < self.text_len:
            c = self.text[self.pos]
            if not (utils.is_valid_name(c) or c.isdigit()):
                break
            self.pos += 1
        lit = self.text[start:self.pos]
        self.pos -= 1 # fix pos
        return lit

    def read_hex_number(self):
        start = self.pos
        self.pos += 2 # skip '0x'
        if self.pos < self.text_len and self.cur_char() == NUM_SEP:
            report.error(
                "separator `_` is only valid between digits in a numeric literal",
                self.get_pos(),
            )
        while self.pos < self.text_len:
            ch = self.cur_char()
            if ch == NUM_SEP and self.text[self.pos + 1] == NUM_SEP:
                report.error(
                    "cannot use `_` consecutively in a numeric literal",
                    self.get_pos()
                )
            if not is_hex_number(ch) and ch != NUM_SEP:
                if not ch.isdigit() and not ch.isalpha():
                    break
                else:
                    report.error(
                        f"this hexadecimal number has unsuitable digit `{self.cur_char()}`",
                        self.get_pos(),
                    )
            self.pos += 1
        if self.text[self.pos - 1] == NUM_SEP:
            report.error(
                "cannot use `_` at the end of a numeric literal", self.get_pos()
            )
        lit = self.text[start:self.pos]
        self.pos -= 1 # fix pos
        return lit

    def read_bin_number(self):
        start = self.pos
        has_wrong_digit = False
        self.pos += 2 # skip '0b'
        if self.pos < self.text_len and self.cur_char() == NUM_SEP:
            report.error(
                "separator `_` is only valid between digits in a numeric literal",
                self.get_pos(),
            )
        while self.pos < self.text_len:
            ch = self.cur_char()
            if ch == NUM_SEP and self.text[self.pos + 1] == NUM_SEP:
                report.error(
                    "cannot use `_` consecutively in a numeric literal",
                    self.get_pos()
                )
            if not is_bin_number(ch) and ch != NUM_SEP:
                if not ch.isdigit() and not ch.isalpha():
                    break
                elif not has_wrong_digit:
                    has_wrong_digit = True
                    report.error(
                        f"this binary number has unsuitable digit `{self.cur_char()}`",
                        self.get_pos(),
                    )
            self.pos += 1
        if self.text[self.pos - 1] == NUM_SEP:
            report.error(
                "cannot use `_` at the end of a numeric literal", self.get_pos()
            )
        lit = self.text[start:self.pos]
        self.pos -= 1 # fix pos
        return lit

    def read_dec_number(self):
        start = self.pos
        while self.pos < self.text_len:
            ch = self.cur_char()
            if ch == NUM_SEP and self.text[self.pos + 1] == NUM_SEP:
                report.error(
                    "cannot use `_` consecutively in a numeric literal",
                    self.get_pos()
                )
            if not ch.isdigit() and ch != NUM_SEP:
                if not ch.isalpha() or ch in ["e", "E"]:
                    break
                else:
                    report.error(
                        f"this number has unsuitable digit `{self.cur_char()}`",
                        self.get_pos(),
                    )
            self.pos += 1
        if self.text[self.pos - 1] == NUM_SEP:
            report.error(
                "cannot use `_` at the end of a numeric literal", self.get_pos()
            )

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
                                    self.get_pos(),
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
                        "float literals should have a digit after the decimal point, "
                        "e.g. `1.0`",
                        self.get_pos(),
                    )
                    self.pos += 1

        # exponential part
        if self.pos < self.text_len and self.text[self.pos] in ["e", "E"]:
            self.pos += 1
            if self.pos < self.text_len and self.text[self.pos] in ["-", "+"]:
                self.pos += 1
            while self.pos < self.text_len:
                c = self.text[self.pos]
                has_suffix = c == "f"
                if not c.isdigit():
                    if has_suffix:
                        self.read_ident()
                    if not c.isalpha():
                        # 6e6.str()
                        break
                    elif not has_suffix:
                        report.error(
                            f"this number has unsuitable digit `{c}`",
                            self.get_pos()
                        )
                self.pos += 1
        lit = self.text[start:self.pos]
        self.pos -= 1 # fix pos
        return lit

    def read_number(self):
        if self.expect("0x", self.pos):
            return self.read_hex_number()
        elif self.expect("0b", self.pos):
            return self.read_bin_number()
        return self.read_dec_number()

    def read_char(self):
        len = 0
        start = self.pos
        backslash = "\\"

        while True:
            self.pos += 1
            if self.pos >= self.text_len:
                break
            if self.cur_char() != backslash:
                len += 1
            double_slash = self.expect("\\\\", self.pos - 2)
            if self.cur_char() == "'" and (
                self.text[self.pos - 1] != backslash or double_slash
            ):
                if double_slash:
                    len += 1
                break
        len -= 1

        ch = self.text[start + 1:self.pos]
        if len != 1:
            if len > 1:
                report.error(
                    "character literal may only contain one codepoint",
                    self.get_pos()
                )
                report.help(
                    "if you meant to write a string literal, use double quotes"
                )
            elif len == 0:
                report.error("empty character literal", self.get_pos())
        return ch

    def read_string(self):
        start = self.pos
        start_char = self.cur_char()
        backslash = "\\"
        backslash_count = 1 if start_char == backslash else 0
        is_raw = self.pos > 0 and self.text[self.pos - 1] == "r"
        n_cr_chars = 0

        while True:
            self.pos += 1
            if self.pos >= self.text_len:
                self.pos = start
                report.error("unfinished string literal", self.get_pos())
                return ""
            c = self.cur_char()
            if c == backslash:
                backslash_count += 1
            # end of string
            if c == '"' and (is_raw or backslash_count % 2 == 0):
                break # handle "\\" at the end
            if c == CR:
                n_cr_chars += 1
            if c == LF:
                self.inc_line_number()
            if c != backslash:
                backslash_count = 0

        lit = ""
        if start <= self.pos:
            lit = self.text[start + 1:self.pos]
            if n_cr_chars > 0:
                lit = lit.replace("\r", "")
        return lit

    def next(self):
        while True:
            cidx = self.tidx
            self.tidx += 1
            if cidx >= len(self.all_tokens):
                return tokens.Token("", Kind.EOF, self.get_pos())
            return self.all_tokens[cidx]
        return tokens.Token("", Kind.EOF, self.get_pos())

    def _next(self):
        while True:
            if self.is_started:
                self.pos += 1
            else:
                self.is_started = True
            self.skip_whitespace()
            if self.pos >= self.text_len:
                return tokens.Token("", Kind.EOF, self.get_pos())

            ch = self.cur_char()
            nextc = self.text[self.pos +
                              1] if self.pos + 1 < self.text_len else chr(0)
            pos = self.get_pos()
            if utils.is_valid_name(ch):
                lit = self.read_ident()
                return tokens.Token(lit, tokens.lookup(lit), pos)
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
                return tokens.Token(
                    self.read_number().replace("_", ""), Kind.Number, pos
                )
            # delimiters and operators
            elif ch == "+":
                if nextc == "+":
                    self.pos += 1
                    return tokens.Token("", Kind.Inc, pos)
                elif nextc == "=":
                    self.pos += 1
                    return tokens.Token("", Kind.PlusAssign, pos)
                return tokens.Token("", Kind.Plus, pos)
            elif ch == "-":
                if nextc == "-":
                    self.pos += 1
                    return tokens.Token("", Kind.Dec, pos)
                elif nextc == "=":
                    self.pos += 1
                    return tokens.Token("", Kind.MinusAssign, pos)
                return tokens.Token("", Kind.Minus, pos)
            elif ch == "*":
                if nextc == "=":
                    self.pos += 1
                    return tokens.Token("", Kind.MultAssign, pos)
                return tokens.Token("", Kind.Mult, pos)
            elif ch == "/":
                if nextc == "/":
                    start_pos = self.pos
                    if self.expect("///", start_pos):
                        start_pos += 3
                        self.ignore_line()
                        line = self.text[start_pos:self.pos].strip()
                        return tokens.Token(line, Kind.DocComment, pos)
                    self.ignore_line()
                    continue
                elif nextc == "*":
                    start_pos = self.pos
                    self.pos += 1
                    while self.pos < self.text_len - 1:
                        self.pos += 1
                        if self.cur_char() == LF:
                            self.inc_line_number()
                            continue
                        elif self.expect("*/", self.pos):
                            self.pos += 1
                            break
                    if self.pos >= self.text_len:
                        self.pos = start_pos
                        report.error("comment not terminated", self.get_pos())
                    continue
                elif nextc == "=":
                    self.pos += 1
                    return tokens.Token("", Kind.DivAssign, pos)
                return tokens.Token("", Kind.Div, pos)
            elif ch == "%":
                if nextc == "=":
                    self.pos += 1
                    return tokens.Token("", Kind.ModAssign, pos)
                return tokens.Token("", Kind.Mod, pos)
            #
            elif ch == "=":
                if nextc == "=":
                    self.pos += 1
                    return tokens.Token("", Kind.Eq, pos)
                elif nextc == ">":
                    self.pos += 1
                    return tokens.Token("", Kind.Arrow, pos)
                return tokens.Token("", Kind.Assign, pos)
            #
            elif ch == "<":
                if nextc == "=":
                    self.pos += 1
                    return tokens.Token("", Kind.Le, pos)
                return tokens.Token("", Kind.Lt, pos)
            elif ch == ">":
                if nextc == "=":
                    self.pos += 1
                    return tokens.Token("", Kind.Ge, pos)
                return tokens.Token("", Kind.Gt, pos)
            #
            elif ch == ".":
                if nextc == "." and self.text[self.pos + 2] == ".":
                    self.pos += 2
                    return tokens.Token("", Kind.Ellipsis, pos)
                elif nextc == ".":
                    self.pos += 1
                    return tokens.Token("", Kind.DotDot, pos)
                return tokens.Token("", Kind.Dot, pos)
            elif ch == ",":
                return tokens.Token("", Kind.Comma, pos)
            elif ch == ":":
                if nextc == ":":
                    self.pos += 1
                    return tokens.Token("", Kind.DoubleColon, pos)
                return tokens.Token("", Kind.Colon, pos)
            elif ch == ";":
                return tokens.Token("", Kind.Semicolon, pos)
            elif ch == "?":
                return tokens.Token("", Kind.Question, pos)
            elif ch == "$":
                return tokens.Token("", Kind.Dollar, pos)
            elif ch == "@":
                return tokens.Token("", Kind.At, pos)
            elif ch == "&":
                if nextc == "=":
                    self.pos += 1
                    return tokens.Token("", Kind.AmpAssign, pos)
                return tokens.Token("", Kind.Amp, pos)
            elif ch == "!":
                if (
                    nextc == "i" and self.text[self.pos + 2] in ("s", "n")
                    and self.text[self.pos + 3].isspace()
                ):
                    self.pos += 2
                    ch2 = self.cur_char()
                    if ch2 == "s":
                        return tokens.Token("", Kind.KeyNotIs, pos)
                    elif ch2 == "n":
                        return tokens.Token("", Kind.KeyNotIn, pos)
                elif nextc == "=":
                    self.pos += 1
                    return tokens.Token("", Kind.Ne, pos)
                return tokens.Token("", Kind.Bang, pos)
            elif ch == "|":
                if nextc == "=":
                    self.pos += 1
                    return tokens.Token("", Kind.PipeAssign, pos)
                return tokens.Token("", Kind.Pipe, pos)
            elif ch == "~":
                return tokens.Token("", Kind.BitNot, pos)
            elif ch == "^":
                if nextc == "=":
                    self.pos += 1
                    return tokens.Token("", Kind.XorAssign, pos)
                return tokens.Token("", Kind.Xor, pos)
            elif ch == "#":
                return tokens.Token("", Kind.Hash, pos)
            #
            elif ch == "{":
                return tokens.Token("", Kind.Lbrace, pos)
            elif ch == "}":
                return tokens.Token("", Kind.Rbrace, pos)
            elif ch == "[":
                return tokens.Token("", Kind.Lbracket, pos)
            elif ch == "]":
                return tokens.Token("", Kind.Rbracket, pos)
            elif ch == "(":
                return tokens.Token("", Kind.Lparen, pos)
            elif ch == ")":
                return tokens.Token("", Kind.Rparen, pos)
            # characters and strings
            elif ch == "'":
                return tokens.Token(self.read_char(), Kind.Char, pos)
            elif ch == '"':
                return tokens.Token(self.read_string(), Kind.String, pos)
            else:
                report.error(f"invalid character `{ch}`", pos)
                break
        return tokens.Token("", Kind.EOF, self.get_pos())
