package demo

if x > 10 {}
when x {
    if x do y
} else when y {
    #partial switch f{}
}

main :: proc(x: struct{foo:[dynamic]int}) where x > 10{
    
}

// A COMMENT!
/*BLOCK COMMENT!*/
z :: struct #align(4) #max_field_align(4) #packed{

}

X :: struct {
    f: proc "c" (x: struct{foo:[dynamic]int})
}
