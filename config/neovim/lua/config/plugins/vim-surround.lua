local M = {}

function M.setup()
    vim.g.surround_no_mappings = 1
    vim.g.surround_no_insert_mappings = 0

    vim.api.nvim_set_keymap("n", "ds", "<Plug>Dsurround", {})
    vim.api.nvim_set_keymap("n", "cs", "<Plug>Csurround", {})
    vim.api.nvim_set_keymap("n", "cS", "<Plug>CSurround", {})
    vim.api.nvim_set_keymap("n", "ys", "<Plug>Ysurround", {})
    vim.api.nvim_set_keymap("n", "yS", "<Plug>YSurround", {})
    vim.api.nvim_set_keymap("n", "yss", "<Plug>Yssurround", {})
    vim.api.nvim_set_keymap("n", "ySs", "<Plug>YSsurround", {})
    vim.api.nvim_set_keymap("n", "ySS", "<Plug>YSsurround", {})
    vim.api.nvim_set_keymap("x", "S", "<Plug>Vsurround", {})
    vim.api.nvim_set_keymap("x", "gS", "<Plug>Vgsurround", {})
end

return M
