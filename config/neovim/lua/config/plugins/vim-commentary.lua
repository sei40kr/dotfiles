local M = {}

function M.setup()
    vim.api.nvim_set_keymap("x", "gc", "<Plug>Commentary", {})
    vim.api.nvim_set_keymap("n", "gc", "<Plug>Commentary", {})
    vim.api.nvim_set_keymap("o", "gc", "<Plug>Commentary", {})
    vim.api.nvim_set_keymap("n", "gcc", "<Plug>CommentaryLine", {})
    vim.api.nvim_set_keymap("n", "cgc", "<Plug>ChangeCommentary", {})
    vim.api.nvim_set_keymap("n", "gcu", "<Plug>Commentary<Plug>Commentary", {})
end

return M
