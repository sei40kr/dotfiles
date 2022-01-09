local M = {}

function M.config()
    require("nvim-autopairs").setup({
        check_ts = true,
        map_c_h = true,
        map_c_w = true,
    })
end

return M
