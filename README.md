# Ansify
ANSI highlighter for odin

Usually used for discord highlighting, using the special `ansi` language. Unfortunately wont work on mobile

Usage:
	`ansify.exe [i] [o] [-ci] [-co] [-discord] [-never-assume-cast] [-no-keyword-builtins] [-quiet]`
Flags:
```
	-i:<Handle>           | Input file. Optional, reads from stdin if omitted
	-o:<Handle>           | Output file. Optional, dumps to stdout if omitted
	                      |
	-ci                   | Whether to copy the input from the clipboard (Windows only)
	-co                   | Whether to copy the output to the clipboard (Windows only)
	-discord              | Wrap output in a discord codeblock. Warns to stderr if output is over 2000 chars
	-never-assume-cast    | Always assume foo(bar) is a function
	-no-keyword-builtins  | Don't give special highlighting to builtin types
	-quiet                | Never print to stdout
```