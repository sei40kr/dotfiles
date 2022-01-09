local M = {}

function M.config()
    local cb = require("diffview.config").diffview_callback

    require("diffview").setup({
        icons = {
            folder_closed = " ",
            folder_open = " ",
        },
        file_panel = { listing_style = "list" },
        key_bindings = {
            view = { q = cb("close") },
            file_panel = { q = cb("close") },
            file_history_panel = { q = cb("close") },
        },
    })
end

return M
