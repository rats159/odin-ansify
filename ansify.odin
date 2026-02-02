package ansify

import "base:intrinsics"
import "base:runtime"
import "core:flags"
import "core:fmt"
import "core:mem"
import "core:odin/ast"
import "core:odin/parser"
import "core:odin/tokenizer"
import "core:os"
import "core:reflect"
import "core:slice"
import "core:strings"

parser_error_handler :: proc(pos: tokenizer.Pos, msg: string, args: ..any) {
	fmt.eprintf("%s(%d:%d): ", pos.file, pos.line, pos.column)
	fmt.eprintf(msg, ..args)
	fmt.eprintf("\n")
	if strings.contains(msg, "package") {
		fmt.eprintln("Did you mean to use `-partial`?")
	}
}

// Doesnt report errors about file scope because `-partial` breaks the rules
lenient_error_handler :: proc(pos: tokenizer.Pos, msg: string, args: ..any) {
	if strings.contains(msg, "in the file scope") {
		return
	}
	fmt.eprintf("%s(%d:%d): ", pos.file, pos.line, pos.column)
	fmt.eprintf(msg, ..args)
	fmt.eprintf("\n")
	if strings.contains(msg, "package") {
		fmt.eprintln("Did you mean to use `-partial`?")
	}
}

// minor change from `core:odin/parser.parse_file`
parse_partial :: proc(p: ^parser.Parser, file: ^ast.File) -> bool {
	p.prev_tok = {}
	p.curr_tok = {}
	p.expr_level = 0
	p.allow_range = false
	p.allow_in_expr = false
	p.in_foreign_block = false
	p.allow_type = false
	p.lead_comment = nil
	p.line_comment = nil

	p.tok.flags += {.Insert_Semicolon}

	p.file = file
	tokenizer.init(&p.tok, file.src, file.fullpath, p.err)
	if p.tok.ch <= 0 {
		return true
	}


	parser.advance_token(p)
	parser.consume_comment_groups(p, p.prev_tok)

	for p.curr_tok.kind != .EOF {
		if p.curr_tok.kind == .Comment {
			parser.consume_comment_groups(p, p.prev_tok)
		} else if p.curr_tok.kind == .File_Tag {
			append(&p.file.tags, p.curr_tok)
			parser.advance_token(p)
		} else {
			break
		}
	}

	if p.curr_tok.kind == .Package {
		parser.advance_token(p) // package
		parser.advance_token(p) // name
	}

	if p.file.syntax_error_count > 0 {
		return false
	}

	p.file.decls = make([dynamic]^ast.Stmt)

	for p.curr_tok.kind != .EOF {
		stmt := parser.parse_stmt(p)
		if stmt != nil {
			if _, ok := stmt.derived.(^ast.Empty_Stmt); !ok {
				append(&p.file.decls, stmt)
				if es, es_ok := stmt.derived.(^ast.Expr_Stmt); es_ok && es.expr != nil {
					if _, pl_ok := es.expr.derived.(^ast.Proc_Lit); pl_ok {
						parser.error(p, stmt.pos, "procedure literal evaluated but not used")
					}
				}
			}
		}
	}

	return true
}

Injection :: struct {
	at:    int,
	type:  Type,
	start: bool,
}

Type :: enum {
	None,
	Keyword,
	Constant,
	String,
	Procedure,
	Comment,
	Number,
	Type,
	Directive,
}

colors := [Type]string {
	.None      = `[0m`,
	.Keyword   = `[31m`,
	.Constant  = `[35m`,
	.String    = `[32m`,
	.Procedure = `[34m`,
	.Comment   = `[30m`,
	.Number    = `[36m`,
	.Type      = `[33m`,
	.Directive = `[31m`, // Same as keywords, but you can change it if you want
}
// printing this file turns the whole thing red because of the ansi codes
// this resets it
//[0m

Options :: struct {
	i:                   os.Handle `args:"pos=0,file=r" usage:"Input file. Optional, reads from stdin if omitted"`,
	o:                   os.Handle `args:"pos=1,file=cw" usage:"Output file. Optional, dumps to stdout if omitted"`,
	co:                  bool `usage:"Whether to copy the output to the clipboard (Windows only)"`,
	ci:                  bool `usage:"Whether to copy the input from the clipboard (Windows only)"`,
	quiet:               bool `usage:"Never print to stdout"`,
	never_assume_cast:   bool `usage:"Always assume foo(bar) is a function"`,
	no_keyword_builtins: bool `usage:"Don't give special highlighting to builtin types"`,
	discord:             bool `usage:"Wrap output in a discord codeblock. Warns to stderr if output is over 2000 chars"`,
}

opt: Options

main :: proc() {
	style: flags.Parsing_Style = .Odin

	flags.parse_or_exit(&opt, os.args, style)

	when ODIN_OS != .Windows {
		if opt.ci {
			panic(
				"Direct to clipboard is only supported on Windows. Consider piping stdout to your clipboard on other systems.",
			)
		}

		if opt.co {
			panic("Direct from clipboard is only supported on Windows.")
		}
	}

	text: string

	if opt.i != 0 {
		data := os.read_entire_file(opt.i) or_else panic("Failed to read file")
		text = string(data)
		if len(text) == 0 {
			fmt.eprintln("[ERROR] File has no text on it")
			os.exit(1)
		}
	} else if opt.ci {
		text = get_from_clipboard()
		if len(text) == 0 {
			fmt.eprintln("[ERROR] Clipboard has no text on it")
			os.exit(1)
		}
	} else {
		text = read_whole_stdin()
		if len(text) == 0 {
			fmt.eprintln("[ERROR] stdin is empty")
			os.exit(1)
		}

	}

	assert(text != "")

	p := parser.default_parser()

	p.err = lenient_error_handler

	file := ast.File {
		src = text,
	}

	ok := parse_partial(&p, &file)

	if !ok {
		fmt.eprintln("Parser error")
		os.exit(1)
	}

	injections: [dynamic]Injection

	v := &ast.Visitor{visit = proc(v: ^ast.Visitor, node: ^ast.Node) -> ^ast.Visitor {
			if node == nil do return v
			injections := (^[dynamic]Injection)(v.data)
			parse_node(injections, node)
			return v
		}, data = &injections}

	ast.walk(v, file.pkg_decl)
	for decl in file.decls {
		ast.walk(v, decl)
	}

	tok := tokenizer.Tokenizer{}
	tokenizer.init(&tok, text, "")

	// Second (dumber) pass over tokens for things that weren't caught
	//   This is mainly for info not stored inside the AST such as `do` tokens
	for {
		tk := tokenizer.scan(&tok)
		if tk.kind == .EOF {
			break
		}
		#partial switch tk.kind {
		case .Else, .Do, .Package:
			write_token(&injections, tk, .Keyword)
		case .Comment:
			write_token(&injections, tk, .Comment)
		case .Hash:
			name := tokenizer.scan(&tok)
			assert(name.kind == .Ident, "Directives should start with a #")
			write_token(&injections, tk, .Directive)
			write_token(&injections, name, .Directive)
		case .File_Tag:
			write_token(&injections, tk, .Directive)
		case .Integer, .Float, .Imag:
			write_token(&injections, tk, .Number)
		case .String:
			write_token(&injections, tk, .String)
		}
	}

	slice.sort_by(injections[:], proc(a, b: Injection) -> bool {
		return a.at < b.at
	})

	flattened_injections: [dynamic]Injection
	type_stack: [dynamic]Type
	append(&type_stack, Type.None)

	for inj in injections {
		if inj.start {
			append(&flattened_injections, inj)
			append(&type_stack, inj.type)
		} else {
			pop(&type_stack)
			append(
				&flattened_injections,
				Injection{at = inj.at, start = inj.start, type = type_stack[len(type_stack) - 1]},
			)
		}
	}

	optimized_injections: [dynamic]Injection
	for inj, i in flattened_injections {
		if len(optimized_injections) == 0 {
			append(&optimized_injections, inj)
			continue
		}

		if optimized_injections[len(optimized_injections) - 1].type == inj.type {
			continue
		}

		if i < len(flattened_injections) - 1 {
			next := flattened_injections[i + 1]
			domain := text[inj.at:next.at]
			all_whitespace := true
			for c in domain {
				if !strings.is_space(c) {
					all_whitespace = false
					break
				}
			}

			if all_whitespace {
				continue
			}
		}

		append(&optimized_injections, inj)
	}

	last := 0
	segments: [dynamic]string

	for injection in optimized_injections {
		append(&segments, text[last:injection.at])
		last = injection.at
	}

	append(&segments, text[last:])
	assert(len(segments) == len(optimized_injections) + 1)

	builder: strings.Builder

	if opt.discord {
		strings.write_string(&builder, "```ansi\n")
	}

	for i in 0 ..< len(optimized_injections) {
		strings.write_string(&builder, segments[i])
		fmt.sbprint(&builder, colors[optimized_injections[i].type])
	}
	strings.write_string(&builder, segments[len(segments) - 1])

	if opt.discord {
		strings.write_string(&builder, "\n```")
	}

	final_output := strings.to_string(builder)

	if opt.o == 0 && !opt.quiet {
		fmt.println(final_output)
	} else {
		os.write(opt.o, transmute([]u8)(final_output))
	}

	if len(final_output) > 2000 && opt.discord {
		fmt.eprintln("[WARN] Output over 2000 characters")
	}

	if opt.co {
		copy_to_clipboard(final_output)
	}
}

when ODIN_OS == .Windows {
	read_whole_stdin :: proc() -> string {
		buffer := make([dynamic]byte, len = 0, cap = 4096)

		for {
			#no_bounds_check n, err := os.read(os.stdin, buffer[len(buffer):cap(buffer)])
			if n == 0 || err == .BROKEN_PIPE do break
			if err != nil {
				panic("OS Error")
			}

			(^runtime.Raw_Dynamic_Array)(&buffer).len += n

			if len(buffer) == cap(buffer) {
				reserve(&buffer, cap(buffer) * 2)
			}
		}

		return string(buffer[:])
	}
} else {
	read_whole_stdin :: proc() -> string {
		buffer := make([dynamic]byte, len = 0, cap = 4096)

		for {
			#no_bounds_check n, err := os.read(os.stdin, buffer[len(buffer):cap(buffer)])
			if n == 0 do break
			if err != nil {
				panic("OS Error")
			}

			(^runtime.Raw_Dynamic_Array)(&buffer).len += n

			if len(buffer) == cap(buffer) {
				reserve(&buffer, cap(buffer) * 2)
			}
		}

		return string(buffer[:])
	}
}

parse_node :: proc(injections: ^[dynamic]Injection, node: ^ast.Node) {
	switch type in node.derived {
	case ^ast.Package_Decl:
		write_token(injections, type.token, .Keyword)
	case ^ast.Import_Decl:
		write_token(injections, type.import_tok, .Keyword)
		write_token(injections, type.relpath, .String)
	case ^ast.Foreign_Import_Decl:
		write_token(injections, type.foreign_tok, .Keyword)
		write_token(injections, type.import_tok, .Keyword)
		for path in type.fullpaths {
			write_node(injections, path, .String)
		}
	case ^ast.Foreign_Block_Decl:
		write_token(injections, type.tok, .Keyword)
	case ^ast.Comment_Group:
		for comment in type.list {
			write_token(injections, comment, .Comment)
		}
	case ^ast.Value_Decl:
		if len(type.values) != 0 do for name in type.names {
			#partial switch t2 in type.values[0].derived_expr {
			case ^ast.Proc_Lit:
				write_node(injections, name, .Procedure)
				if t2.where_token != {} {
					write_token(injections, t2.where_token, .Keyword)
				}
			case ^ast.Proc_Group:
				write_node(injections, name, .Procedure)
			case ^ast.Struct_Type, ^ast.Enum_Type, ^ast.Union_Type, ^ast.Distinct_Type:
				write_type(injections, type.values[0])
				write_node(injections, name, .Type)
			case ^ast.Ident:
				// This is imperfect, but impossible without another full
				//   process of the AST
				write_node(injections, name, .Type)
				if is_builtin_type(t2.name) {
					write_type(injections, type.values[0])
				}
			case:
				if !type.is_mutable {
					write_node(injections, name, .Constant)
				}
			}
		}

		if type.type != nil {
			write_type(injections, type.type)
		}
	case ^ast.Or_Else_Expr:
		write_token(injections, type.token, .Keyword)
	case ^ast.Or_Branch_Expr:
		write_token(injections, type.token, .Keyword)
	case ^ast.Or_Return_Expr:
		write_token(injections, type.token, .Keyword)
	case ^ast.Proc_Lit:
		write_type(injections, type.type)
	case ^ast.Basic_Lit:
		#partial switch type.tok.kind {
		case .Integer, .Float, .Imag:
			write_token(injections, type.tok, .Number)
		case .String, .Rune:
			write_token(injections, type.tok, .String)
		case:
			unimplemented(fmt.aprint("Literal type", type.tok))
		}
	case ^ast.If_Stmt:
		write_pos(injections, type.if_pos.offset, len("if"), .Keyword)
		if type.else_stmt != nil {
			write_pos(injections, type.else_pos.offset, len("else"), .Keyword)
		}

	case ^ast.When_Stmt:
		write_pos(injections, type.when_pos.offset, len("when"), .Keyword)
	case ^ast.Ternary_If_Expr:
		if type.op1.kind == .Question {
			break
		}
		write_token(injections, type.op1, .Keyword)
		write_token(injections, type.op2, .Keyword)

	case ^ast.Ternary_When_Expr:
		write_token(injections, type.op1, .Keyword)
		write_token(injections, type.op2, .Keyword)
	case ^ast.Return_Stmt:
		write_pos(injections, type.pos.offset, len("return"), .Keyword)
	case ^ast.Type_Switch_Stmt:
		write_pos(injections, type.switch_pos.offset, len("switch"), .Keyword)
		write_pos(
			injections,
			type.tag.derived_stmt.(^ast.Assign_Stmt).op.pos.offset,
			len("in"),
			.Keyword,
		)

		stmts := type.body.derived.(^ast.Block_Stmt).stmts
		for stmt in stmts {
			case_ := stmt.derived.(^ast.Case_Clause)
			for expr in case_.list {
				write_type(injections, expr)
			}
		}
	case ^ast.Switch_Stmt:
		write_pos(injections, type.switch_pos.offset, len("switch"), .Keyword)

	case ^ast.Case_Clause:
		write_pos(injections, type.case_pos.offset, len("case"), .Keyword)

	case ^ast.Implicit:
		write_token(injections, type.tok, .Keyword)
	case ^ast.Implicit_Selector_Expr:
		write_pos(injections, type.field.pos.offset, len(type.field.name), .Constant)
	case ^ast.Field:
		for name in type.names {
			if _, ok := name.derived.(^ast.Poly_Type); ok {
				write_type(injections, name)
			}
		}
		if type.type != nil {
			write_type(injections, type.type)
		}
	case ^ast.Call_Expr:
		expr, call_purpose := extract_call(type.expr)
		if expr == nil {
			break
		}
		switch call_purpose {
		case .Call:
			write_node(injections, expr, .Procedure)
		case .Cast:
			if opt.never_assume_cast {
				write_node(injections, expr, .Procedure)
			} else {
				write_type(injections, expr)
			}
		case .Directive:
			write_node(injections, expr, .Directive)
		}
	case ^ast.Comp_Lit:
		if type.type != nil {
			write_type(injections, type.type)
		}
	case ^ast.Basic_Directive:
		write_pos(injections, type.tok.pos.offset, len(type.name) + 1, .Directive)
	case ^ast.Ident:
		switch type.name {
		case "nil", "true", "false":
			write_pos(injections, type.pos.offset, len(type.name), .Keyword)
		}
	case ^ast.For_Stmt:
		write_pos(injections, type.for_pos.offset, len("for"), .Keyword)
	case ^ast.Range_Stmt:
		write_pos(injections, type.for_pos.offset, len("for"), .Keyword)
		write_pos(injections, type.in_pos.offset, len("in"), .Keyword)
	case ^ast.Unroll_Range_Stmt:
		write_pos(injections, type.unroll_pos.offset - 1, len("#unroll"), .Keyword)
		write_pos(injections, type.for_pos.offset, len("for"), .Keyword)
		write_pos(injections, type.in_pos.offset, len("in"), .Keyword)
	case ^ast.Branch_Stmt:
		write_token(injections, type.tok, .Keyword)
	case ^ast.Defer_Stmt:
		write_pos(injections, type.pos.offset, len("defer"), .Keyword)
	case ^ast.Proc_Group:
		write_token(injections, type.tok, .Keyword)
		for expr in type.args {
			ident := expr.derived_expr.(^ast.Ident)
			write_pos(injections, ident.pos.offset, len(ident.name), .Procedure)
		}
	case ^ast.Using_Stmt:
		write_pos(injections, type.pos.offset, len("using"), .Keyword)
	case ^ast.Attribute:
		write_pos(injections, type.pos.offset, len("@"), .Keyword)
		for elem in type.elems {
			#partial switch t in elem.derived_expr {
			case ^ast.Ident:
				write_pos(injections, t.pos.offset, len(t.name), .Keyword)
			case ^ast.Field_Value:
				ident := t.field.derived.(^ast.Ident)
				write_pos(injections, ident.pos.offset, len(ident.name), .Keyword)
			case:
				panic("Unreachable type in attribute element")
			}
		}
	case ^ast.Package:
		write_pos(injections, type.pos.offset, len("package"), .Keyword)
	case ^ast.Tag_Expr:
		write_pos(injections, type.op.pos.offset, len(type.name) + 1, .Directive)
	case ^ast.Tag_Stmt:
		write_pos(injections, type.op.pos.offset, len(type.name) + 1, .Directive)
	case ^ast.Helper_Type:
		write_pos(injections, type.pos.offset, len("helper") + 1, .Directive)
	case ^ast.Auto_Cast:
		write_token(injections, type.op, .Keyword)
	case ^ast.Inline_Asm_Expr:
		// Inline ASM is weird right now, I don't think it's supported properly, but it's in the parser
		write_token(injections, type.tok, .Keyword)
	case ^ast.Type_Cast:
		write_token(injections, type.tok, .Keyword)
		write_type(injections, type.type)
	case ^ast.Type_Assertion:
		write_type(injections, type.type)

	// Empty
	case ^ast.Empty_Stmt:
	// Thin wrappers over other nodes handled by the walker
	case ^ast.Block_Stmt,
	     ^ast.Field_List,
	     ^ast.Selector_Expr,
	     ^ast.Index_Expr,
	     ^ast.Unary_Expr,
	     ^ast.Binary_Expr,
	     ^ast.Paren_Expr,
	     ^ast.Assign_Stmt,
	     ^ast.Ellipsis,
	     ^ast.Expr_Stmt,
	     ^ast.Slice_Expr,
	     ^ast.Deref_Expr,
	     ^ast.Matrix_Index_Expr,
	     ^ast.Selector_Call_Expr:
	// Types. This sometimes results in duplicated injections,
	//  but the optimizer removes them
	case ^ast.Dynamic_Array_Type,
	     ^ast.Proc_Type,
	     ^ast.Pointer_Type,
	     ^ast.Array_Type,
	     ^ast.Map_Type,
	     ^ast.Union_Type,
	     ^ast.Distinct_Type,
	     ^ast.Poly_Type,
	     ^ast.Typeid_Type,
	     ^ast.Bit_Set_Type,
	     ^ast.Matrix_Type,
	     ^ast.Bit_Field_Type,
	     ^ast.Struct_Type,
	     ^ast.Enum_Type,
	     ^ast.Multi_Pointer_Type:
		expr: ast.Expr
		expr.expr_base = node^
		mem.copy(
			&expr.derived_expr,
			&node.derived,
			intrinsics.type_union_tag_offset(type_of(expr.derived_expr)),
		)
		reflect.set_union_variant_typeid(
			expr.derived_expr,
			reflect.union_variant_typeid(node.derived),
		)

		write_type(injections, &expr)

	// Fields, handled by write_type
	case ^ast.Field_Value, ^ast.Bit_Field_Field:

	case ^ast.File:
		panic("An ast.File made it directly into parsing")
	case ^ast.Bad_Stmt:
		fmt.panicf(
			"Unhandled parser error made it into highlighting. Between: %v:%v and %v:%v",
			type.pos.line,
			type.pos.column,
			type.end.line,
			type.end.column,
		)
	case ^ast.Bad_Expr:
		fmt.panicf(
			"Unhandled parser error made it into highlighting. Between: %v:%v and %v:%v",
			type.pos.line,
			type.pos.column,
			type.end.line,
			type.end.column,
		)
	case ^ast.Bad_Decl:
		fmt.panicf(
			"Unhandled parser error made it into highlighting. Between: %v:%v and %v:%v",
			type.pos.line,
			type.pos.column,
			type.end.line,
			type.end.column,
		)
	case ^ast.Relative_Type:
		panic("#relative types have been removed from the language")
	case ^ast.Undef:
		write_node(injections, type.node, .Keyword)
	}
}

write_type :: proc(injections: ^[dynamic]Injection, expr: ^ast.Expr, loc := #caller_location) {
	assert(expr != nil, "Internal error: nil expression was passed to write_type")
	#partial switch t in expr.derived_expr {
	case ^ast.Ident:
		if is_builtin_type(t.name) && !opt.no_keyword_builtins {
			write_pos(injections, t.pos.offset, len(t.name), .Keyword)
		} else {
			write_pos(injections, t.pos.offset, len(t.name), .Type)
		}
	case ^ast.Pointer_Type:
		write_type(injections, t.elem)
	case ^ast.Multi_Pointer_Type:
		write_type(injections, t.elem)
	case ^ast.Selector_Expr:
		write_type(injections, t.field)
	case ^ast.Dynamic_Array_Type:
		write_pos(injections, t.dynamic_pos.offset, len("dynamic"), .Keyword)
		write_type(injections, t.elem)
	case ^ast.Distinct_Type:
		write_pos(injections, t.pos.offset, len("distinct"), .Keyword)
		write_type(injections, t.type)
	case ^ast.Union_Type:
		write_pos(injections, t.pos.offset, len("union"), .Keyword)
		for variant in t.variants {
			write_type(injections, variant)
		}
		if t.poly_params != nil {
			for param in t.poly_params.list {
				for name in param.names {
					write_type(injections, name)
				}
			}
		}
	case ^ast.Struct_Type:
		write_pos(injections, t.pos.offset, len("struct"), .Keyword)
		for field in t.fields.list {
			write_type(injections, field.type)
			if field.tag != {} {
				write_token(injections, field.tag, .String)
			}
		}
		if t.poly_params != nil {
			for param in t.poly_params.list {
				for name in param.names {
					write_type(injections, name)
				}
			}
		}
	case ^ast.Map_Type:
		write_pos(injections, t.tok_pos.offset, len("map"), .Keyword)
		write_type(injections, t.key)
		write_type(injections, t.value)
	case ^ast.Array_Type:
		write_type(injections, t.elem)
		if t.len != nil {
			#partial switch length_type in t.len.derived_expr {
			case ^ast.Ident:
				write_type(injections, t.len)
			case ^ast.Poly_Type:
				write_node(injections, t.len, .Constant)
			case ^ast.Binary_Expr:
				// identifier in binary expr in array length is a constant, e.g. [SIZE + 10]int
				if const, ok := length_type.left.derived.(^ast.Ident); ok {
					write_node(injections, const, .Constant)
				}

				if const, ok := length_type.right.derived.(^ast.Ident); ok {
					write_node(injections, const, .Constant)
				}
			}
		}
	case ^ast.Ellipsis:
		write_type(injections, t.expr)
	case ^ast.Typeid_Type:
		write_pos(injections, t.pos.offset, len("typeid"), .Keyword)
		if t.specialization != nil {
			write_type(injections, t.specialization)
		}
	case ^ast.Poly_Type:
		write_pos(injections, t.dollar.offset, len(t.type.name) + 1, .Type)
		if t.specialization != nil {
			write_type(injections, t.specialization)
		}
	case ^ast.Call_Expr:
		write_type(injections, t.expr)
		for arg in t.args {
			write_type(injections, arg)
		}
	case ^ast.Bit_Set_Type:
		write_pos(injections, t.pos.offset, len("bit_set"), .Keyword)
	case ^ast.Bit_Field_Type:
		write_pos(injections, t.pos.offset, len("bit_field"), .Keyword)
	case ^ast.Matrix_Type:
		write_pos(injections, t.pos.offset, len("matrix"), .Keyword)
		write_type(injections, t.elem)
	case ^ast.Enum_Type:
		write_pos(injections, t.pos.offset, len("enum"), .Keyword)
		for field in t.fields {
			write_node(injections, field, .Constant)
		}
	case ^ast.Proc_Type:
		write_pos(injections, t.pos.offset, len("proc"), .Keyword)
	case ^ast.Unary_Expr:
		assert(t.op.kind == .Question, ".? should be the only unary expr type")
	case:
		unimplemented(fmt.aprint("Type type", expr.derived))

	}
}

// What do we think this call actually is?
Call_Target :: enum {
	Call,
	Cast,
	Directive,
}

extract_call :: proc(base: ^ast.Expr) -> (^ast.Expr, Call_Target) {
	#partial switch type in base.derived_expr {
	case ^ast.Selector_Expr:
		return extract_call(type.field)
	case ^ast.Call_Expr:
		return extract_call(type.expr)
	case ^ast.Ident:
		// Capitals are probably a type?
		if type.name[0] >= 'A' && type.name[0] <= 'Z' {
			return base, .Cast
		}
		if is_builtin_type(type.name) {
			return base, .Cast
		}
		return base, .Call
	case ^ast.Paren_Expr:
		return extract_call(type.expr)
	case ^ast.Index_Expr:
		return nil, .Call
	case ^ast.Pointer_Type,
	     ^ast.Multi_Pointer_Type,
	     ^ast.Struct_Type,
	     ^ast.Matrix_Type,
	     ^ast.Array_Type,
	     ^ast.Bit_Field_Type,
	     ^ast.Map_Type,
	     ^ast.Bit_Set_Type,
	     ^ast.Poly_Type,
	     ^ast.Enum_Type,
	     ^ast.Union_Type,
	     ^ast.Proc_Type,
	     ^ast.Dynamic_Array_Type,
	     ^ast.Distinct_Type:
		return base, .Cast
	case ^ast.Basic_Directive, ^ast.Tag_Expr:
		return base, .Directive
	case:
		unimplemented(fmt.aprint("Unimplemented call type", base.derived_expr))
	}
}
// odinfmt: disable
is_builtin_type :: proc(t: string) -> bool {
	switch t {
	case "int", "i128", "i64", "i32", "i16", "i8",
	     "i128le", "i64le", "i32le", "i16le",
	     "i128be", "i64be", "i32be", "i16be",
	     "uint", "u128", "u64", "u32", "u16", "u8", "byte",
	     "u128le", "u64le", "u32le", "u16le",
	     "u128be", "u64be", "u32be", "u16be",
		 "f64", "f32", "f16",
		 "f64be", "f32be", "f16be",
		 "f64le", "f32le", "f16le",
	     "quaternion256", "quaternion128", "quaternion64",
	     "complex128", "complex64", "complex32",
	  	 "bool", "b64", "b32", "b16", "b8",
	     "string", "cstring", "string16", "cstring16", "rune",
		 "any", "rawptr", "uintptr",
		 "typeid":
		return true
	}
	return false
}
// odinfmt: enable

write_pos :: proc(injections: ^[dynamic]Injection, pos: int, len: int, type: Type) {
	append(injections, Injection{pos, type, true})
	append(injections, Injection{pos + len, type, false})
}

write_node :: proc(injections: ^[dynamic]Injection, decl: ast.Node, type: Type) {
	append(injections, Injection{decl.pos.offset, type, true})
	append(injections, Injection{decl.end.offset, type, false})
}

write_token :: proc(injections: ^[dynamic]Injection, tk: tokenizer.Token, type: Type) {
	append(injections, Injection{tk.pos.offset, type, true})
	append(injections, Injection{tk.pos.offset + len(tk.text), type, false})
}

