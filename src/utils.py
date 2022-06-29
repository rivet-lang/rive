# Copyright (C) 2022 The Rivet Team. All rights reserved.
# Use of this source code is governed by an MIT license
# that can be found in the LICENSE file.

import sys, subprocess

from . import colors

VERSION = "0.1.0"

INVALID_ESCAPES = ["(", "{", "$", "`", "."]
BACKSLASH = chr(92)
BACKSLASH_R = chr(13)
BACKSLASH_N = chr(10)
DOUBLE_QUOTE = chr(34)
DOUBLE_ESCAPE = "\\\\"

def commit_hash():
	return execute("git", "log", "-n", "1", '--pretty=format:%h').out

def full_version():
	commit_date = execute("git", "log", "-n", "1", '--pretty=format:%h %as').out
	return f"rivetc {VERSION} ({commit_date})"

class ProcessResult:
	def __init__(self, out, err, exit_code):
		self.out = out
		self.err = err
		self.exit_code = exit_code

def execute(*args):
	res = subprocess.run(args, capture_output = True)
	return ProcessResult(
	    res.stdout.decode().strip(),
	    res.stderr.decode().strip(), res.returncode
	)

class Builder:
	def __init__(self):
		self.buf = ""

	def write(self, txt):
		self.buf += txt

	def writeln(self, txt = ""):
		if txt == "":
			self.buf += "\n"
		else:
			self.buf += f"{txt}\n"

	def __len__(self):
		return len(self.buf)

	def __repr__(self):
		return self.buf

	def __str__(self):
		return self.buf

class CompilerError(Exception):
	pass

def index_any(s, chars):
	for i, ss in enumerate(s):
		for c in chars:
			if c == ss:
				return i
	return -1

def bytestr(string):
	buf = bytearray((' ' * len(string)).encode('ascii'))
	buf = string.encode('utf-8')
	return (buf, len(buf))

def eprint(*s, end = "\n"):
	print(*s, end = end, file = sys.stderr)

def error(msg):
	bg = colors.bold(f'rivetc: {colors.red("error:")}')
	eprint(f"{bg} {msg}")
	exit(1)

def is_valid_name(ch):
	return (ch >= "A" and ch <= "Z") or (ch >= "a" and ch <= "z") or ch == "_"

def smart_quote(str, raw: bool):
	len_ = len(str)
	if len_ == 0:
		return ""
	if len_ < 256:
		is_pure = True
		for i in range(0, len_):
			ch = str[i]
			if ((ch >= chr(37) and ch <= chr(90))
			    or (ch >= chr(95) and ch <= chr(126))
			    or (ch in (" ", "!", "#", "[", "]"))):
				# safe punctuation + digits + big latin letters, small latin
				# letters + more safe punctuation, important punctuation exceptions,
				# that are not placed conveniently in a consequitive span in
				# the ASCII table.
				continue
			is_pure = False
			break
		if is_pure:
			return str
	result = ""
	pos = -1
	last = chr(0)
	current = chr(0)
	next = chr(0)
	skip_next = False
	while True:
		pos += 1
		if skip_next:
			skip_next = False
			pos += 1
		if pos >= len_:
			break
		last = current
		current = str[pos]
		if pos + 1 < len_:
			next = str[pos + 1]
		else:
			next = 0
		if current == DOUBLE_QUOTE:
			current = 0
			result += BACKSLASH
			result += DOUBLE_QUOTE
			continue
		if current == BACKSLASH:
			if raw:
				result += DOUBLE_ESCAPE
				continue
			if next == BACKSLASH:
				# escaped backslash - keep as is
				skip_next = True
				result += DOUBLE_ESCAPE
				continue
			if next != chr(0):
				if raw:
					skip_next = True
					result += DOUBLE_ESCAPE
					continue
				if next in INVALID_ESCAPES:
					skip_next = True
					result += next
					continue
				# keep all valid escape sequences
				skip_next = True
				result += current
				result += next
				continue
		if current == BACKSLASH_N:
			# keep newlines in string
			current = chr(0)
			result += BACKSLASH
			result += "n"
			continue
		if current == BACKSLASH_R and next == BACKSLASH_N:
			result += current
			result += next
			current = chr(0)
			skip_next = True
			continue
		if not raw:
			if current == BACKSLASH_R and next == BACKSLASH_N:
				# Windows style new line \r\n
				skip_next = True
				result += BACKSLASH
				result += "n"
				continue
		result += current
	return result

class OrderedDepMap:
	def __init__(self, keys = list(), data = {}):
		self.keys = keys
		self.data = data

	def set(self, name, deps):
		if name not in self.data:
			self.keys.append(name)
		self.data[name] = deps

	def add(self, name, deps):
		d = self.get(name)
		for dep in deps:
			if dep not in d:
				d.append(dep)
		self.set(name, d)

	def get(self, name):
		return self.data[name] if name in self.data else []

	def delete(self, name):
		if name not in self.data:
			raise KeyError(f"OrderedDepMap.delete: no such key `{name}`")
		for i, _ in enumerate(self.keys):
			if self.keys[i] == name:
				self.keys.pop(i)
				break
		self.data.pop(name)

	def apply_diff(self, name, deps):
		diff = []
		deps_of_name = self.get(name)
		for dep in deps_of_name:
			if dep not in deps:
				diff.append(dep)
		self.set(name, diff)

	def size(self):
		return len(self.data)

class DepGraphNode:
	def __init__(self, name, deps):
		self.name = name
		self.deps = deps

class NodeNames:
	def __init__(self, is_cycle = dict(), names = dict()):
		self.is_cycle = is_cycle
		self.names = names

	def is_part_of_cycle(self, name, already_seen):
		seen = False
		new_already_seen = already_seen.copy()
		if name in self.is_cycle:
			return self.is_cycle[name], new_already_seen

		if name in already_seen:
			new_already_seen.append(name)
			self.is_cycle[name] = True
			return True, new_already_seen

		new_already_seen.append(name)
		deps = self.names[name] if name in self.names else []
		if len(deps) == 0:
			self.is_cycle[name] = False
			return False, new_already_seen

		for d in deps:
			d_already_seen = new_already_seen.copy()
			seen, d_already_seen = self.is_part_of_cycle(d, d_already_seen)
			if seen:
				new_already_seen = d_already_seen.copy()
				self.is_cycle[name] = True
				return True, new_already_seen
		self.is_cycle[name] = False
		return False, new_already_seen

class DepGraph:
	def __init__(self, acyclic = True, nodes = list()):
		self.acyclic = acyclic
		self.nodes = nodes

	def add(self, name, deps):
		self.nodes.append(DepGraphNode(name, deps))

	def resolve(self):
		node_names = OrderedDepMap()
		node_deps = OrderedDepMap()
		for node in self.nodes:
			node_names.add(node.name, node.deps)
			node_deps.add(node.name, node.deps)
		iterations = 0
		resolved = DepGraph()
		resolved.nodes = [] # fix bug
		while node_deps.size() != 0:
			iterations += 1
			ready_set = []
			for name in node_deps.keys:
				if len(node_deps.get(name)) == 0:
					ready_set.append(name)
			if len(ready_set) == 0:
				g = DepGraph()
				g.acyclic = False
				for name in node_deps.keys:
					g.add(name, node_names.get(name))
				return g
			for name in ready_set:
				node_deps.delete(name)
				resolved.add(name, node_names.get(name))
			for name in node_deps.keys:
				node_deps.apply_diff(name, ready_set)
		return resolved

	def last_node(self):
		return self.nodes[-1]

	def display(self):
		out = []
		for node in self.nodes:
			for dep in node.deps:
				out.append(f" * {node.name} -> {dep}")
		return "\n".join(out)

	def display_cycles(self):
		seen = False
		out = []
		nn = NodeNames()
		for node in self.nodes:
			nn.names[node.name] = node.deps
		for k, _ in nn.names.items():
			cycle_names = []
			if k in nn.is_cycle:
				continue
			seen, cycle_names = nn.is_part_of_cycle(k, cycle_names)
			if seen:
				out.append(" * " + " -> ".join(cycle_names))
		return "\n".join(out)
