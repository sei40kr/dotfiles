local M = {}

local function get_project_root()
    local project_root, _ = require("project_nvim.project").get_project_root()
    return project_root
end

function M.find_file_in_project()
    local telescope_builtin = require("telescope.builtin")

    local success, _ = pcall(telescope_builtin.git_files)
    if not success then
        telescope_builtin.find_files({
            cwd = get_project_root(),
            hidden = true,
        })
    end
end

function M.search_project()
    require("telescope.builtin").live_grep({ cwd = get_project_root() })
end

return M
