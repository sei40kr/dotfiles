local M = {}

function M.config()
    require("nvim-treesitter.configs").setup {
        highlight = {enable = true},
        indent = {enable = true},
        refactor = {
            highlight_definitions = {enable = true}
        },
        autotag = {enable = true},
        context_commentstring = {enable = true},
        rainbow = {
            enable = true,
            disable = {"help", "NvimTree", "toggleterm", "Trouble"}
        }
    }
end

return M
