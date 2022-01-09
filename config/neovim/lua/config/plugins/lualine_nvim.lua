local M = {}

function M.config()
    require("lualine").setup({
        options = {
            section_separators = "",
            component_separators = "",
            disabled_filetypes = { "NvimTree" },
        },
        sections = {
            lualine_b = {
                "branch",
                {
                    "diagnostics",
                    symbols = {
                        error = " ",
                        warn = " ",
                        info = " ",
                        hint = " ",
                    },
                },
            },
        },
    })
end

return M
