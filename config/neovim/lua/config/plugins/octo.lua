local M = {}

function M.config()
    require("octo").setup({
        right_bubble_delimiter = "",
        left_bubble_delimiter = "",
    })
end

return M
