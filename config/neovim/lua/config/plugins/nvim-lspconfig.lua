local M = {}

function M.config()
    local nvim_lsp = require("lspconfig")

    local on_attach = function(client, bufnr)
        if client.resolved_capabilities.completion then
            vim.api.nvim_buf_set_option(
                bufnr,
                "omnifunc",
                "v:lua.vim.lsp.omnifunc"
            )
        end

        require("config.keymaps.lsp").setup(client, bufnr)
    end
    local capabilities = require("cmp_nvim_lsp").update_capabilities(
        vim.lsp.protocol.make_client_capabilities()
    )

    local servers = {
        clangd = {},
        cssls = {},
        -- emmet_ls = {},
        eslint = {},
        gopls = {},
        hls = {},
        html = {
            on_attach = function(client, bufnr)
                client.resolved_capabilities.document_formatting = false
                client.resolved_capabilities.document_range_formatting = false
            end,
        },
        jsonls = {
            on_attach = function(client, bufnr)
                client.resolved_capabilities.document_formatting = false
                client.resolved_capabilities.document_range_formatting = false
            end,
        },
        metals = {},
        ocamllsp = {},
        pyright = {},
        r_language_server = {},
        rust_analyzer = {},
        sorbet = {},
        sourcekit = {},
        sumneko_lua = require("lua-dev").setup({}),
        -- tailwindcss = {},
        terraformls = {},
        tsserver = {
            on_attach = function(client, bufnr)
                client.resolved_capabilities.document_formatting = false
                client.resolved_capabilities.document_range_formatting = false
            end,
        },
        vuels = {},
        yamlls = {},
    }

    for server, opts in pairs(servers) do
        if opts.on_attach ~= nil then
            opts.on_attach = (function(old_on_attach)
                return function(client, bufnr)
                    old_on_attach(client, bufnr)
                    on_attach(client, bufnr)
                end
            end)(opts.on_attach)
        end

        nvim_lsp[server].setup(vim.tbl_deep_extend("force", {
            on_attach = on_attach,
            capabilities = capabilities,
            flags = { debouce_text_changes = 150 },
        }, opts))
    end

    for severity, sign in pairs({
        Error = "",
        Hint = "",
        Info = "",
        Warn = "",
    }) do
        local name = "DiagnosticSign" .. severity
        vim.fn.sign_define(name, { text = sign, texthl = name, numhl = "" })
    end

    vim.lsp.handlers["textDocument/publishDiagnostics"] = vim.lsp.with(
        vim.lsp.diagnostic.on_publish_diagnostics,
        {
            underline = true,
            virtual_text = {
                spacing = 4,
                prefix = "●",
            },
            severity_sort = true,
        }
    )
end

return M
