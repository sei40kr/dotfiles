vim.opt.guifont = "@fontFamily@:h@fontSize@"
vim.opt.mouse = "a"

vim.cmd [[
GuiTabline 0
GuiPopupmenu 0
]]

vim.api.nvim_set_keymap("n", "<RightMouse>", "<Cmd>call GuiShowContextMenu()<CR>", {silent=true})
vim.api.nvim_set_keymap("i", "<RightMouse>", "<Cmd>call GuiShowContextMenu()<CR>", {silent=true})
vim.api.nvim_set_keymap("x", "<RightMouse>", "<Cmd>call GuiShowContextMenu()<CR>", {silent=true})
vim.api.nvim_set_keymap("s", "<RightMouse>", "<Cmd>call GuiShowContextMenu()<CR>", {silent=true})
