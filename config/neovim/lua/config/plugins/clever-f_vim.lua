local M = {}

function M.setup()
    vim.g.clever_f_ignore_case = true
    vim.g.clever_f_smart_case = true
    vim.g.clever_f_not_overwrites_standard_mappings = true

    vim.api.nvim_set_keymap("n", "f", "<Plug>(clever-f-f)", {})
    vim.api.nvim_set_keymap("x", "f", "<Plug>(clever-f-f)", {})
    vim.api.nvim_set_keymap("o", "f", "<Plug>(clever-f-f)", {})
    vim.api.nvim_set_keymap("n", "F", "<Plug>(clever-f-F)", {})
    vim.api.nvim_set_keymap("x", "F", "<Plug>(clever-f-F)", {})
    vim.api.nvim_set_keymap("o", "F", "<Plug>(clever-f-F)", {})
    vim.api.nvim_set_keymap("n", "t", "<Plug>(clever-f-t)", {})
    vim.api.nvim_set_keymap("x", "t", "<Plug>(clever-f-t)", {})
    vim.api.nvim_set_keymap("o", "t", "<Plug>(clever-f-t)", {})
    vim.api.nvim_set_keymap("n", "T", "<Plug>(clever-f-T)", {})
    vim.api.nvim_set_keymap("x", "T", "<Plug>(clever-f-T)", {})
    vim.api.nvim_set_keymap("o", "T", "<Plug>(clever-f-T)", {})
end

return M
