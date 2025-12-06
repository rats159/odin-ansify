#+build !windows
package ansify

copy_to_clipboard :: proc(data: string) {
    panic("Direct to clipboard is only supported on Windows. Consider piping stdout to your clipboard on other systems.")
}

get_from_clipboard :: proc() -> string {
    panic("Reading from clipboard is only supported on Windows.")
}
