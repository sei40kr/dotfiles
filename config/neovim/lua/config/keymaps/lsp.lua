local M = {}

function M.setup(client, bufnr)
    local wk = require("which-key")

    wk.register({
        ["<Leader>c"] = {
            l = {
                name = "+lsp",
                F = {
                    name = "+folders",
                    a = {
                        "<Cmd>lua vim.lsp.buf.add_workspace_folder()<CR>",
                        "add folder",
                    },
                    r = {
                        "<Cmd>lua vim.lsp.buf.remove_workspace_folder()<CR>",
                        "remove folder",
                    },
                },
            },
            x = {
                "<Cmd>Trouble document_diagnostics<CR>",
                "List errors in current buffer",
            },
            X = {
                "<Cmd>Trouble workspace_diagnostics<CR>",
                "List errors in current workspace",
            },
        },
        ["[e"] = {
            "<Cmd>lua vim.diagnostic.goto_prev()<CR>",
            "Jump to previous error",
        },
        ["]e"] = {
            "<Cmd>lua vim.diagnostic.goto_next()<CR>",
            "Jump to next error",
        },
    }, { buffer = bufnr })

    if client.resolved_capabilities.code_action then
        wk.register({
            ["<Leader>ca"] = {
                '<Cmd>lua require("telescope.builtin").lsp_code_actions()<CR>',
                "Execute code action",
            },
        }, { buffer = bufnr })
        wk.register({
            ["<Leader>ca"] = {
                ':lua require("telescope.builtin").lsp_range_code_actions()<CR>',
                "Execute code action",
            },
        }, { mode = "x", buffer = bufnr })
    end

    if client.resolved_capabilities.document_formatting then
        wk.register({
            ["<Leader>cf"] = {
                "<Cmd>lua vim.lsp.buf.formatting()<CR>",
                "Format buffer",
            },
        }, { buffer = bufnr })
    end

    if client.resolved_capabilities.document_range_formatting then
        wk.register({
            ["<Leader>cf"] = {
                ":lua vim.lsp.buf.range_formatting()<CR>",
                "Format region",
            },
        }, { mode = "x", buffer = bufnr })
    end

    if client.resolved_capabilities.find_references then
        vim.api.nvim_buf_set_keymap(
            bufnr,
            "n",
            "gD",
            "<Cmd>Trouble lsp_references<CR>",
            { noremap = true }
        )
    end

    if client.resolved_capabilities.goto_definition then
        vim.api.nvim_buf_set_keymap(
            bufnr,
            "n",
            "gd",
            '<Cmd>lua require("telescope.builtin").lsp_definitions()<CR>',
            { noremap = true }
        )
    end

    if client.resolved_capabilities.hover then
        vim.api.nvim_buf_set_keymap(
            bufnr,
            "n",
            "K",
            "<Cmd>lua vim.lsp.buf.hover()<CR>",
            { noremap = true }
        )
    end

    if client.resolved_capabilities.implementation then
        wk.register({
            ["<Leader>ci"] = {
                '<Cmd>lua require("telescope.builtin").lsp_implementations()<CR>',
                "Find implementations",
            },
        }, { buffer = bufnr })
    end

    if client.resolved_capabilities.rename then
        wk.register({
            ["<Leader>cr"] = {
                "<Cmd>lua vim.lsp.buf.rename()<CR>",
                "LSP Rename",
            },
        }, { buffer = bufnr })
    end

    if client.resolved_capabilities.type_definition then
        wk.register({
            ["<Leader>ct"] = {
                '<Cmd>lua require("telescope.builtin").lsp_type_definitions()<CR>',
                "Find type definition",
            },
        }, { buffer = bufnr })
    end

    if client.resolved_capabilities.workspace_symbol then
        wk.register({
            ["<Leader>cj"] = {
                '<Cmd>lua require("telescope.builtin").lsp_workspace_symbols()<CR>',
                "Jump to symbol in current workspace",
            },
            ["<Leader>cJ"] = {
                '<Cmd>lua require("telescope.builtin").lsp_dynamic_workspace_symbols()<CR>',
                "Jump to symbol in all workspace",
            },
        }, { buffer = bufnr })
    end
end

return M
