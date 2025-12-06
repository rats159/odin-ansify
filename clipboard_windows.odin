package ansify

import "core:mem"
import "core:strings"
import "core:sys/windows"

copy_to_clipboard :: proc(text: string) {
	windows.OpenClipboard(nil)
	defer windows.CloseClipboard()

	windows.EmptyClipboard()


	// utf16


	wide_text := utf8_to_utf16(text)

	wide_pointer := windows.GlobalAlloc(windows.GMEM_MOVEABLE, (len(wide_text) + 1) * 2)

	wide_data := windows.GlobalLock(auto_cast wide_pointer)
	mem.copy(wide_data, raw_data(wide_text), len(wide_text) * 2)
	([^]u16)(wide_data)[len(wide_text)] = 0
	windows.GlobalUnlock(auto_cast wide_pointer)
	delete(wide_text)

	windows.SetClipboardData(windows.CF_UNICODETEXT, auto_cast wide_pointer)


	// ansi


	pointer := windows.GlobalAlloc(windows.GMEM_MOVEABLE, len(text) + 1)

	data := windows.GlobalLock(auto_cast pointer)
	mem.copy(data, raw_data(text), len(text))
	([^]u8)(data)[len(text)] = 0
	windows.GlobalUnlock(auto_cast pointer)

	windows.SetClipboardData(windows.CF_TEXT, auto_cast pointer)
}

get_from_clipboard :: proc() -> string {
	windows.OpenClipboard(nil)
	defer windows.CloseClipboard()

	data := windows.GetClipboardData(windows.CF_UNICODETEXT)
	if data == nil {
		data = windows.GetClipboardData(windows.CF_TEXT)
		if data == nil {
			return ""
		}

		lock := windows.GlobalLock(auto_cast data)
		defer windows.GlobalUnlock(auto_cast data)
		return strings.clone(string(cstring(lock)))
	} else {
		str := cstring16(windows.GlobalLock(auto_cast data))
		defer windows.GlobalUnlock(auto_cast data)

		len := windows.WideCharToMultiByte(windows.CP_UTF8, 0, str, -1, nil, 0, nil, nil)
		if len > 0 {
			buffer := make([]u8, len - 1)
			windows.WideCharToMultiByte(
				windows.CP_UTF8,
				0,
				str,
				-1,
				raw_data(buffer),
				len,
				nil,
				nil,
			)
			return string(buffer)
		}
	}

	return ""
}

utf8_to_utf16 :: proc(utf8: string) -> string16 {
	wide_len := windows.MultiByteToWideChar(windows.CP_UTF8, 0, raw_data(utf8), -1, nil, 0)
	wide_data := make([]u16, wide_len)
	wide_str := string16(wide_data)
	windows.MultiByteToWideChar(
		windows.CP_UTF8,
		0,
		raw_data(utf8),
		i32(len(utf8)),
		raw_data(wide_str),
		wide_len,
	)
	return wide_str
}

