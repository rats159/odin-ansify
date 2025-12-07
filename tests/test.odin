package demo

import "core:fmt"

when x {
    if x do y
} else when y {
    #partial switch f{}
}
// Some comment
Type :: struct #align(16) {
    x,y: [dynamic]int
}

Some_Const :: 10

Foo :: enum {
    A, B, C
}

main :: proc() {
    fmt.println("Hi!")
}