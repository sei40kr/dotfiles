local M = {}

function M.config()
    require("nvim-treesitter.configs").setup({
        highlight = { enable = true },
        indent = { enable = true },
        incremental_selection = {
            enable = true,
            keymaps = {
                init_selection = "C-=",
                node_incremental = "C-=",
                scope_incremental = "<Nop>",
                node_decremental = "C--",
            },
        },
        refactor = {
            highlight_definitions = { enable = true },
            smart_rename = {
                enable = true,
                keymaps = { smart_rename = "<Leader>cr" },
            },
        },
        autotag = { enable = true },
        context_commentstring = { enable = true },
        move = {
            enable = true,
            goto_next_start = {
                ["]]"] = "@class.outer",
                ["]c"] = "@comment.outer",
                ["]m"] = "@function.outer",
            },
            goto_next_end = {
                ["]["] = "@class.outer",
                ["]M"] = "@function.outer",
            },
            goto_previous_start = {
                ["[["] = "@class.outer",
                ["[c"] = "@comment.outer",
                ["[m"] = "@function.outer",
            },
            goto_previous_end = {
                ["[]"] = "@class.outer",
                ["[M"] = "@function.outer",
            },
        },
        rainbow = {
            enable = true,
            disable = { "help", "NvimTree", "toggleterm", "Trouble" },
        },
    })
end

return M
