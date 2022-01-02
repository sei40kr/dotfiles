local M = {}

function M.config()
    local nvim_lsp = require("lspconfig")
    local cmp_nvim_lsp = require("cmp_nvim_lsp")
    local luadev = require("lua-dev").setup({})
    local wk = require("which-key")

    local on_attach = function(client, bufnr)
        vim.api.nvim_buf_set_option(bufnr, "omnifunc", "v:lua.vim.lsp.omnifunc")

        vim.api.nvim_buf_set_keymap(
            bufnr,
            "n",
            "gd",
            '<Cmd>lua require("telescope.builtin").lsp_definitions()<CR>',
            {noremap = true}
        )
        vim.api.nvim_buf_set_keymap(bufnr, "n", "gD", "<Cmd>Trouble lsp_references<CR>", {noremap = true})
        vim.api.nvim_buf_set_keymap(bufnr, "n", "K", "<Cmd>lua vim.lsp.buf.hover()<CR>", {noremap = true})

        wk.register(
            {
                ["<Leader>c"] = {
                    a = {'<Cmd>lua require("telescope.builtin").lsp_code_actions()<CR>', "LSP Execute code action"},
                    d = {"gd", "Jump to definition", noremap = false},
                    D = {"gD", "Jump to references", noremap = false},
                    f = {"<Cmd>lua vim.lsp.buf.formatting()<CR>", "Format buffer"},
                    i = {'<Cmd>lua require("telescope.builtin").lsp_implementations()<CR>', "Find implementations"},
                    j = {
                        '<Cmd>lua require("telescope.builtin").lsp_workspace_symbols()<CR>',
                        "Jump to symbol in current workspace"
                    },
                    J = {
                        '<Cmd>lua require("telescope.builtin").lsp_dynamic_workspace_symbols()<CR>',
                        "Jump to symbol in all workspace"
                    },
                    k = {"K", "Jump to documentation", noremap = false},
                    l = {
                        name = "+lsp",
                        F = {
                            name = "+folders",
                            a = {"<Cmd>lua vim.lsp.buf.add_workspace_folder()<CR>", "add folder"},
                            r = {"<Cmd>lua vim.lsp.buf.remove_workspace_folder()<CR>", "remove folder"}
                        }
                    },
                    r = {"<Cmd>lua vim.lsp.buf.rename()<CR>", "LSP Rename"},
                    t = {'<Cmd>lua require("telescope.builtin").lsp_type_definitions()<CR>', "Find type definition"},
                    x = {"<Cmd>Trouble lsp_document_diagnostics<CR>", "List errors"},
                    X = {"<Cmd>Trouble lsp_workspace_diagnostics<CR>", "Errors list"}
                },
                ["[e"] = {"<Cmd>lua vim.diagnostic.goto_prev()<CR>", "Jump to previous error"},
                ["]e"] = {"<Cmd>lua vim.diagnostic.goto_next()<CR>", "Jump to next error"}
            },
            {buffer = bufnr}
        )
        wk.register(
            {
                name = "+code",
                a = {':lua require("telescope.builtin").lsp_range_code_actions()<CR>', "LSP Execute code action"},
                f = {":lua vim.lsp.buf.range_formatting()<CR>", "Format region"}
            },
            {
                mode = "x",
                buffer = bufnr,
                prefix = "<Leader>c"
            }
        )
    end
    local capabilities = cmp_nvim_lsp.update_capabilities(vim.lsp.protocol.make_client_capabilities())

    local servers = {
        clangd = {},
        cssls = {},
        -- emmet_ls = {},
        eslint = {},
        gopls = {},
        hls = {},
        html = {},
        jsonls = {},
        metals = {},
        ocamllsp = {},
        pyright = {},
        r_language_server = {},
        rust_analyzer = {},
        sorbet = {},
        sourcekit = {},
        sumneko_lua = luadev,
        -- tailwindcss = {},
        terraformls = {},
        tsserver = {},
        vuels = {},
        yamlls = {}
    }

    for server, opts in pairs(servers) do
        nvim_lsp[server].setup(
            vim.tbl_deep_extend(
                "force",
                {
                    on_attach = on_attach,
                    capabilities = capabilities,
                    flags = {debouce_text_changes = 150}
                },
                opts
            )
        )
    end

    for severity, sign in pairs(
        {
            Error = "",
            Hint = "",
            Information = "",
            Warning = ""
        }
    ) do
        local name = "LspDiagnosticsSign" .. severity
        vim.fn.sign_define(name, {text = sign, texthl = hl, numhl = ""})
    end

    vim.lsp.handlers["textDocument/publishDiagnostics"] =
        vim.lsp.with(
        vim.lsp.diagnostic.on_publish_diagnostics,
        {
            underline = true,
            virtual_text = {
                spacing = 4,
                prefix = "●"
            },
            severity_sort = true
        }
    )
end

return M
