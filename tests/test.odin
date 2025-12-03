package demo

import "core:fmt"

main :: proc() {
    fmt.println("Odin highlighting!")
    Foo :: struct {
        x: [dynamic]f32,
        y: int
    }
    
    CONSTANT :: 10
    Enum :: enum {
        A, B, C
    }
}