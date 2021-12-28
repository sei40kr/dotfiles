local M = {}

function M.config()
    local telescope = require("telescope")
    local actions = require("telescope.actions")

    telescope.setup {
        defaults = {
            mappings = {
                i = {
                    ["<C-j>"] = actions.move_selection_previous,
                    ["<C-k>"] = actions.move_selection_next,
                    ["<Esc>"] = actions.close,
                    jk = actions.close
                }
            }
        },
        extensions = {
            project = {
                base_dirs = {{"~/ghq", max_depth = 4}}
            }
        }
    }
end

return M
