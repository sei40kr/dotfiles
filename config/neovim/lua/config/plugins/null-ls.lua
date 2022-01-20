local M = {}

function M.config()
    local null_ls = require("null-ls")
    null_ls.setup({
        sources = {
            null_ls.builtins.code_actions.shellcheck,
            null_ls.builtins.formatting.nixfmt,
            null_ls.builtins.formatting.prettier,
            null_ls.builtins.formatting.prettier_d_slim,
            null_ls.builtins.formatting.shfmt,
            null_ls.builtins.formatting.stylua,
            null_ls.builtins.diagnostics.shellcheck,
        },
        on_attach = function(client, bufnr)
            if client.resolved_capabilities.completion then
                vim.api.nvim_buf_set_option(
                    bufnr,
                    "omnifunc",
                    "v:lua.vim.lsp.omnifunc"
                )
            end

            require("config.keymaps.lsp").setup(client, bufnr)
        end,
    })
end

return M
