local M = {}

function M.config()
    local diffview = require("diffview")
    local cb = require("diffview.config").diffview_callback

    diffview.setup {
        use_icons = true,
        file_panel = {listing_style = "list"},
        key_bindings = {
            view = {q = cb("close")},
            file_panel = {q = cb("close")},
            file_history_panel = {q = cb("close")}
        }
    }
end

return M
