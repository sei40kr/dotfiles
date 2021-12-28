local M = {}

function M.config()
    require("bufferline").setup {
        options = {
            indicator_icon = "▍ ",
            offsets = {{filetype = "NvimTree"}},
            separator_style = {"", ""}
        }
    }
end

return M
