local M = {}

function M.config()
    local nvim_tree = require("nvim-tree")

    vim.g.nvim_tree_git_hl = 0
    vim.g.nvim_tree_special_files = {}
    vim.g.nvim_tree_respect_buf_cwd = 1
    vim.g.nvim_tree_show_icons = {
        git = 0,
        folders = 1,
        files = 1,
        folder_arrows = 1,
    }
    vim.g.nvim_tree_icons = {
        default = " ",
        symlink = " ",
        folder = {
            default = " ",
            open = " ",
            empty = " ",
            empty_open = " ",
            symlink = " ",
            symlink_open = " ",
        },
    }

    nvim_tree.setup({
        update_cwd = true,
        update_focused_file = {
            enable = true,
            update_cwd = true,
        },
        view = {
            width = 35,
            mappings = {
                custom_only = false,
                -- TODO Add custom keymaps
                list = {},
            },
            signcolumn = "no",
        },
    })
end

return M
