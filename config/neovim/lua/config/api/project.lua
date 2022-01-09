local M = {}

function M.get_project_root()
    local project_root, _ = require("project_nvim.project").get_project_root()
    return project_root
end

return M
