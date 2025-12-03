package ansify
// hiello
import "core:fmt"
import "core:odin/ast"
import "core:odin/parser"
import "core:odin/tokenizer"
import "core:os/os2"
import "core:slice"
import "core:strings"
import "core:text/regex"


Injection :: struct {
	at:    int,
	type:  Type,
	start: bool,
}

xx :: 10

Type :: enum {
	NONE,
	KEYWORD,
	CONSTANT,
	STRING,
	PROCEDURE,
	COMMENT,
	INTEGER,
	TYPE,
	DIRECTIVE,
}

colors := [Type]string {
	.NONE      = `[0m`,
	.KEYWORD   = `[31m`,
	.CONSTANT  = `[35m`,
	.STRING    = `[32m`,
	.PROCEDURE = `[34m`,
	.COMMENT   = `[30m`,
	.INTEGER   = `[36m`,
	.TYPE      = `[33m`,
	.DIRECTIVE = `[31m`,
}

main :: proc() {
	data :=
		os2.read_entire_file(os2.args[1], context.allocator) or_else panic("Failed to read file")
	text := string(data)

	p := parser.default_parser()
	f := ast.File {
		fullpath = os2.args[1],
		src      = text,
	}
	ok := parser.parse_file(&p, &f)

	if !ok {
		panic("Invalid file contents")
	}

	injections: [dynamic]Injection

	v := &ast.Visitor{visit = proc(v: ^ast.Visitor, node: ^ast.Node) -> ^ast.Visitor {
			if node == nil do return v
			injections := (^[dynamic]Injection)(v.data)
			parse_node(injections, node)
			return v
		}, data = &injections}

	ast.walk(v, f.pkg_decl)
	for decl in f.decls {
		ast.walk(v, decl)
	}

	// `do` and `#partial` tokens are never stored, so we need a second pass to find them
	secondary_matcher :=
		regex.create_iterator(text, "\\s(?:do|#partial|else)\\s") or_else panic("regex creation failed")

	// comments
	comment_matcher :=
		regex.create_iterator(text, `\/\*(?:\s|\S)*?\*\/|\/\/[^\n]*`) or_else panic(
			"regex creation failed",
		)
	for match, _ in regex.match_iterator(&secondary_matcher) {
		append(&injections, Injection{type = .KEYWORD, at = match.pos[0][0], start = true})
		append(&injections, Injection{type = .KEYWORD, at = match.pos[0][1], start = false})
	}

	for match, _ in regex.match_iterator(&comment_matcher) {
		append(&injections, Injection{type = .COMMENT, at = match.pos[0][0], start = true})
		append(&injections, Injection{type = .COMMENT, at = match.pos[0][1], start = false})
	}

	slice.sort_by(injections[:], proc(a, b: Injection) -> bool {
		return a.at < b.at
	})

	flattened_injections: [dynamic]Injection
	type_stack: [dynamic]Type
	append(&type_stack, Type.NONE)

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

			//
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
	for i in 0 ..< len(optimized_injections) {
		strings.write_string(&builder, segments[i])
		fmt.sbprint(&builder, colors[optimized_injections[i].type])
	}
	strings.write_string(&builder, segments[len(segments) - 1])

	_ = os2.write_entire_file("out.txt", transmute([]u8)(strings.to_string(builder)))
}

parse_node :: proc(injections: ^[dynamic]Injection, node: ^ast.Node) {
	#partial switch type in node.derived {
	case ^ast.Package_Decl:
		write_token(injections, type.token, .KEYWORD)
	case ^ast.Import_Decl:
		write_token(injections, type.import_tok, .KEYWORD)
		write_token(injections, type.relpath, .STRING)
	case ^ast.Foreign_Import_Decl:
		write_token(injections, type.foreign_tok, .KEYWORD)
		write_token(injections, type.import_tok, .KEYWORD)
		for path in type.fullpaths {
			write_node(injections, path, .STRING)
		}
	case ^ast.Foreign_Block_Decl:
		write_token(injections, type.tok, .KEYWORD)
	case ^ast.Comment_Group:
		for comment in type.list {
			write_token(injections, comment, .COMMENT)
		}
	case ^ast.Value_Decl:
		if len(type.values) != 0 do for name in type.names {
			#partial switch t2 in type.values[0].derived_expr {
			case ^ast.Proc_Lit, ^ast.Proc_Group:
				write_node(injections, name, .PROCEDURE)
			case ^ast.Struct_Type, ^ast.Enum_Type, ^ast.Union_Type, ^ast.Distinct_Type:
				write_type(injections, type.values[0])
				write_node(injections, name, .TYPE)
			case:
				if !type.is_mutable {
					write_node(injections, name, .CONSTANT)
				}
			}
		}

		if type.type != nil {
			write_type(injections, type.type)
		}
	case ^ast.Or_Else_Expr:
		write_token(injections, type.token, .KEYWORD)
	case ^ast.Or_Branch_Expr:
		write_token(injections, type.token, .KEYWORD)
	case ^ast.Or_Return_Expr:
		write_token(injections, type.token, .KEYWORD)
	// case ^ast.Struct_Type:
	// 	write_pos(injections, type.pos.offset, len("struct"), .KEYWORD)
	case ^ast.Enum_Type:

	case ^ast.Proc_Lit:
		write_pos(injections, type.pos.offset, len("proc"), .KEYWORD)
	case ^ast.Basic_Lit:
		#partial switch type.tok.kind {
		case .Integer, .Float, .Imag:
			write_token(injections, type.tok, .INTEGER)
		case .String, .Rune:
			write_token(injections, type.tok, .STRING)
		case:
			unimplemented(fmt.aprint("Literal type", type.tok))
		}
	case ^ast.If_Stmt:
		write_pos(injections, type.if_pos.offset, len("if"), .KEYWORD)
		if type.else_stmt != nil {
			write_pos(injections, type.else_pos.offset, len("else"), .KEYWORD)
		}

	case ^ast.When_Stmt:
		write_pos(injections, type.when_pos.offset, len("when"), .KEYWORD)
	case ^ast.Ternary_If_Expr:
		write_token(injections, type.op1, .KEYWORD)
		write_token(injections, type.op2, .KEYWORD)

	case ^ast.Ternary_When_Expr:
		write_token(injections, type.op1, .KEYWORD)
		write_token(injections, type.op2, .KEYWORD)
	case ^ast.Return_Stmt:
		write_pos(injections, type.pos.offset, len("return"), .KEYWORD)
	case ^ast.Type_Switch_Stmt:
		write_pos(injections, type.switch_pos.offset, len("switch"), .KEYWORD)
		write_pos(
			injections,
			type.tag.derived_stmt.(^ast.Assign_Stmt).op.pos.offset,
			len("in"),
			.KEYWORD,
		)

		stmts := type.body.derived.(^ast.Block_Stmt).stmts
		for stmt in stmts {
			case_ := stmt.derived.(^ast.Case_Clause)
			for expr in case_.list {
				write_type(injections, expr)
			}
		}
	case ^ast.Switch_Stmt:
		write_pos(injections, type.switch_pos.offset, len("switch"), .KEYWORD)

	case ^ast.Case_Clause:
		write_pos(injections, type.case_pos.offset, len("case"), .KEYWORD)

	case ^ast.Implicit:
		// I don't really know what this is?
		#partial switch type.tok.kind {
		case .Context:
			write_token(injections, type.tok, .KEYWORD)
		case:
			unimplemented(fmt.aprint("'Implicit' type", type.tok.kind))
		}
	case ^ast.Implicit_Selector_Expr:
		write_pos(injections, type.field.pos.offset, len(type.field.name), .CONSTANT)
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
		switch call_purpose {
		case .Call:
			write_node(injections, expr, .PROCEDURE)
		case .Cast:
			write_type(injections, expr)
		case .Directive:
			write_node(injections, expr, .DIRECTIVE)
		}
	case ^ast.Comp_Lit:
		if type.type != nil {
			write_type(injections, type.type)
		}
	case ^ast.Basic_Directive:
		write_pos(injections, type.tok.pos.offset, len(type.name) + 1, .DIRECTIVE)
	case ^ast.Ident:
		switch type.name {
		case "nil", "true", "false":
			write_pos(injections, type.pos.offset, len(type.name), .KEYWORD)
		}
	case ^ast.For_Stmt:
		write_pos(injections, type.for_pos.offset, len("for"), .KEYWORD)
	case ^ast.Range_Stmt:
		write_pos(injections, type.for_pos.offset, len("for"), .KEYWORD)
		write_pos(injections, type.in_pos.offset, len("in"), .KEYWORD)
	case ^ast.Unroll_Range_Stmt:
		write_pos(injections, type.unroll_pos.offset - 1, len("#unroll"), .KEYWORD)
		write_pos(injections, type.for_pos.offset, len("for"), .KEYWORD)
		write_pos(injections, type.in_pos.offset, len("in"), .KEYWORD)
	case ^ast.Branch_Stmt:
		write_token(injections, type.tok, .KEYWORD)
	case ^ast.Defer_Stmt:
		write_pos(injections, type.pos.offset, len("defer"), .KEYWORD)
	case ^ast.Proc_Group:
		write_token(injections, type.tok, .KEYWORD)
		for expr in type.args {
			ident := expr.derived_expr.(^ast.Ident)
			write_pos(injections, ident.pos.offset, len(ident.name), .PROCEDURE)
		}
	case ^ast.Using_Stmt:
		write_pos(injections, type.pos.offset, len("using"), .KEYWORD)
	case ^ast.Attribute:
		write_pos(injections, type.pos.offset, len("@"), .KEYWORD)
		for elem in type.elems {
			#partial switch t in elem.derived_expr {
			case ^ast.Ident:
				write_pos(injections, t.pos.offset, len(t.name), .KEYWORD)
			case ^ast.Field_Value:
				ident := t.field.derived.(^ast.Ident)
				write_pos(injections, ident.pos.offset, len(ident.name), .KEYWORD)
			case:
				panic("Unreachable type in attribute element")
			}
		}
	case ^ast.Block_Stmt,
	     ^ast.Dynamic_Array_Type,
	     ^ast.Proc_Type,
	     ^ast.Pointer_Type,
	     ^ast.Field_List,
	     ^ast.Selector_Expr,
	     ^ast.Index_Expr,
	     ^ast.Unary_Expr,
	     ^ast.Binary_Expr,
	     ^ast.Paren_Expr,
	     ^ast.Assign_Stmt,
	     ^ast.Expr_Stmt,
	     ^ast.Type_Assertion,
	     ^ast.Slice_Expr,
	     ^ast.Type_Cast,
	     ^ast.Array_Type,
	     ^ast.Field_Value,
	     ^ast.Map_Type,
	     ^ast.Ellipsis,
	     ^ast.Union_Type,
	     ^ast.Distinct_Type,
	     ^ast.Poly_Type,
	     ^ast.Typeid_Type,
	     ^ast.Deref_Expr,
	     ^ast.Bit_Set_Type,
	     ^ast.Matrix_Type,
	     ^ast.Matrix_Index_Expr,
	     ^ast.Bit_Field_Type,
	     ^ast.Bit_Field_Field,
	     ^ast.Struct_Type:
	// nothing here
	case:
		unimplemented(fmt.aprint("Node type", node.derived))
	}
}

write_type :: proc(injections: ^[dynamic]Injection, expr: ^ast.Expr, loc := #caller_location) {
	if expr == nil {
		fmt.println(loc)
	}
	#partial switch t in expr.derived_expr {
	case ^ast.Ident:
		if is_builtin_type(t.name) {
			write_pos(injections, t.pos.offset, len(t.name), .KEYWORD)
		} else {
			write_pos(injections, t.pos.offset, len(t.name), .TYPE)
		}
	case ^ast.Pointer_Type:
		write_type(injections, t.elem)
	case ^ast.Selector_Expr:
		write_type(injections, t.field)
	case ^ast.Dynamic_Array_Type:
		write_pos(injections, t.dynamic_pos.offset, len("dynamic"), .KEYWORD)
		write_type(injections, t.elem)
	case ^ast.Distinct_Type:
		write_pos(injections, t.pos.offset, len("distinct"), .KEYWORD)
		write_type(injections, t.type)
	case ^ast.Union_Type:
		write_pos(injections, t.pos.offset, len("union"), .KEYWORD)
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
		write_pos(injections, t.pos.offset, len("struct"), .KEYWORD)
		for field in t.fields.list {
			write_type(injections, field.type)
			if field.tag != {} {
				write_token(injections, field.tag, .STRING)
			}
		}
		if t.poly_params != nil {
			for param in t.poly_params.list {
				for name in param.names {
					write_type(injections, name)
				}
			}
		}
		if t.min_field_align != nil {
			fmt.println(t.min_field_align.derived)
		}
	case ^ast.Map_Type:
		write_pos(injections, t.tok_pos.offset, len("map"), .KEYWORD)
		write_type(injections, t.key)
		write_type(injections, t.value)
	case ^ast.Array_Type:
		write_type(injections, t.elem)
		if t.len != nil {
			if _, ok := t.len.derived_expr.(^ast.Ident); ok {
				write_type(injections, t.len)
			}
		}
	case ^ast.Ellipsis:
		write_type(injections, t.expr)
	case ^ast.Typeid_Type:
		write_pos(injections, t.pos.offset, len("typeid"), .KEYWORD)
		if t.specialization != nil {
			write_type(injections, t.specialization)
		}
	case ^ast.Poly_Type:
		write_pos(injections, t.dollar.offset, len(t.type.name) + 1, .TYPE)
		if t.specialization != nil {
			write_type(injections, t.specialization)
		}
	case ^ast.Call_Expr:
		write_type(injections, t.expr)
		for arg in t.args {
			write_type(injections, arg)
		}
	case ^ast.Bit_Set_Type:
		write_pos(injections, t.pos.offset, len("bit_set"), .KEYWORD)
	case ^ast.Bit_Field_Type:
		write_pos(injections, t.pos.offset, len("bit_field"), .KEYWORD)
	case ^ast.Matrix_Type:
		write_pos(injections, t.pos.offset, len("matrix"), .KEYWORD)
		write_type(injections, t.elem)
	case ^ast.Enum_Type:
		write_pos(injections, t.pos.offset, len("enum"), .KEYWORD)
		for field in t.fields {
			write_node(injections, field, .CONSTANT)
		}
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
	case ^ast.Pointer_Type:
		return base, .Cast
	case ^ast.Basic_Directive:
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

