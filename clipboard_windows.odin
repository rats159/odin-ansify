package ansify

import "core:mem"
import "core:sys/windows"

copy_to_clipboard :: proc(data: string) {
	windows.OpenClipboard(nil)
	windows.EmptyClipboard()
	data_pointer := windows.GlobalAlloc(windows.GMEM_MOVEABLE, len(data) + 1)

	lock_data := windows.GlobalLock(windows.HGLOBAL(data_pointer))
	mem.copy(lock_data, raw_data(data), len(data))
	([^]u8)(lock_data)[len(data)] = 0
	windows.GlobalUnlock(windows.HGLOBAL(data_pointer))
	windows.SetClipboardData(windows.CF_TEXT, windows.HANDLE(data_pointer))
}
