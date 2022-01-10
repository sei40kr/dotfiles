local M = {}

function M.setup()
    vim.api.nvim_set_keymap("n", ".", "<Plug>(RepeatDot)", {})
    vim.api.nvim_set_keymap("n", "u", "<Plug>(RepeatUndo)", {})
    vim.api.nvim_set_keymap("n", "U", "<Plug>(RepeatUndoLine)", {})
    vim.api.nvim_set_keymap("n", "<C-r>", "<Plug>(RepeatRedo)", {})
end

return M
