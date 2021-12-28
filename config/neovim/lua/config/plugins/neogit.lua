local M = {}

function M.config()
    local neogit = require("neogit")

    neogit.setup {
        disable_hint = true,
        disable_context_highlighting = true,
        disable_commit_confirmation = true,
        commit_popup = {kind = "tab"},
        signs = {
            section = {"", ""},
            item = {"", ""}
        },
        integrations = {diffview = true}
    }
end

return M
