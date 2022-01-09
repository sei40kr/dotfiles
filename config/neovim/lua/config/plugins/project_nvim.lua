local M = {}

function M.config()
    require("project_nvim").setup({
        manual_mode = true,
        detection_methods = { "pattern" },
        patterns = { ".git" },
    })
end

return M
