local M = {}

function M.config()
    vim.g.minicompletion_disable = true
    vim.g.minicursorword_disable = true
    vim.g.minipairs_disable = true
    vim.g.minisessions_disable = true
    vim.g.ministarter_disable = true
    vim.g.ministatusline_disable = true
    vim.g.minitabline_disable = true
    vim.g.minitrailspace_disable = true

    require("mini.bufremove").setup({})

    require("mini.comment").setup({})

    require("mini.jump").setup({})

    require("mini.surround").setup({
        mappings = {
            add = "",
            delete = "ds",
            find = "",
            find_left = "",
            highlight = "",
            replace = "cs",
        },
    })
    vim.api.nvim_set_keymap(
        "n",
        "ys",
        'luaeval(\'require("mini.surround").operator("add")\')',
        { noremap = true, silent = true, expr = true }
    )
    vim.api.nvim_set_keymap(
        "x",
        "S",
        ':<C-u>lua MiniSurround.add("visual")<CR>',
        { noremap = true, silent = true }
    )
end

return M
