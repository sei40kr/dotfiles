local M = {}

function M.config()
    require("Comment").setup({
        toggler = {
            line = "gcc",
            block = "<Nop>",
        },
        opleader = {
            line = "gc",
            block = "<Nop>",
        },
        mappings = {
            extra = false,
        },
    })
end

return M
