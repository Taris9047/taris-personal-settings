local config = import("micro/config")
local shell = import("micro/shell")

function init()	
	config.TryBindKey("Ctrl-Shift-z", "Redo", true)
	config.TryBindKey("Ctrl=/", "lua:comment.comment", true)
end

function gorun(bp)
	local buf = bp.buf
	if buf:FileType() == "go" then
		shell.RunInteractiveShell("go run " .. buf.Path, true, false)
	end
end
