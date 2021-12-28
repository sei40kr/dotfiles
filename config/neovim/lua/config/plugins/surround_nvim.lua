local M = {}

function M.config()
    require("surround").setup {
        mappings_style = "surround",
        map_insert_mode = false,
        prefix = "S"
    }
end

return M
