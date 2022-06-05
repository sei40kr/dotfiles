local M = {}

function M.config()
    local nvim_tree = require("nvim-tree")

    nvim_tree.setup({
        renderer = {
            highlight_git = false,
            icons = {
                glyphs = {
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
                },
                show = {
                    git = false,
                    folder = true,
                    file = true,
                    folder_arrow = true,
                },
            },
            special_files = {},
        },
        respect_buf_cwd = true,
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
