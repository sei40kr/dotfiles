local M = {}

function M.config()
    vim.g.nvim_tree_git_hl = 1
    vim.g.nvim_tree_special_files = {}
    vim.g.nvim_tree_show_icons = {
        git = 0,
        folders = 1,
        files = 1,
        folder_arrows = 1
    }

    require("nvim-tree").setup(
        {
            update_cwd = true,
            view = {
                width = 35,
                mappings = {
                    custom_only = false,
                    -- TODO Add custom keymaps
                    list = {}
                }
            }
        }
    )
end

return M
