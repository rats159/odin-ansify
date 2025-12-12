package demo

import "core:fmt"

when x {
	if x do y
} else when y {
	#partial switch f {}
}
// Some comment
Type :: struct #align (16) {
	x, y: [dynamic]int,
}

x := cast([dynamic]int)z
x := transmute([dynamic]int)z

x := y.([dynamic]int)

Some_Const :: 10

Foo :: enum {
	A,
	B,
	C,
}

main :: proc() {
	fmt.println("Hi!")
	x := Type{}
}

enum_array: [Foo]u8


double :: proc(data: [$N]$T) -> [size_of([N]u8) * 2]T {
	arr: [N * 2]T

	for item, i in data {
		arr[i] = item
		arr[i + N] = item
	}

	return arr
}


Size :: size_of(struct {
		a, b: [dynamic]int,
	})

Integer :: i64

types_as_values :: proc() {
	x := new(int)
	y := make([]i32)
	z := new([10 + Size]int)

	foo := typeid_of(struct {
			x: int,
			y: int,
		})
}

