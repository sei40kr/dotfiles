local M = {}

function M.config()
    local cmp = require("cmp")

    cmp.setup(
        {
            mapping = {
                ["<C-j>"] = cmp.mapping(cmp.mapping.select_next_item()),
                ["<C-k>"] = cmp.mapping(cmp.mapping.select_prev_item()),
                ["<CR>"] = function(fallback)
                    if cmp.visible() then
                        cmp.confirm()
                    else
                        fallback()
                    end
                end,
                ["<Down>"] = cmp.mapping(cmp.mapping.select_next_item()),
                ["<Up>"] = cmp.mapping(cmp.mapping.select_prev_item())
            },
            snippet = {
                expand = function(args)
                    require("luasnip").lsp_expand(args.body)
                end
            },
            formatting = {format = require("lspkind").cmp_format()},
            sources = cmp.config.sources(
                {
                    {name = "nvim_lsp"},
                    {name = "luasnip"}
                },
                {
                    {name = "omni"},
                    {name = "path"},
                    {name = "buffer"},
                    {name = "luasnip"}
                }
            )
        }
    )

    cmp.setup.cmdline(
        ":",
        {
            sources = cmp.config.sources({{name = "path"}}, {{name = "cmdline"}})
        }
    )

    _G.vimrc.cmp = {
        buffer = function()
            cmp.complete(
                {
                    config = {
                        sources = {{name = "buffer"}}
                    }
                }
            )
        end,
        omni = function()
            cmp.complete(
                {
                    config = {
                        sources = {{name = "omni"}}
                    }
                }
            )
        end,
        path = function()
            cmp.complete(
                {
                    config = {
                        sources = {{name = "path"}}
                    }
                }
            )
        end,
        spell = function()
            cmp.complete(
                {
                    config = {
                        sources = {{name = "spell"}}
                    }
                }
            )
        end
    }
    vim.api.nvim_set_keymap("i", "<C-x><C-f>", "<Cmd>lua vimrc.cmp.path()<CR>", {noremap = true})
    vim.api.nvim_set_keymap("i", "<C-x><C-k>", "<Cmd>lua vimrc.cmp.buffer()<CR>", {noremap = true})
    vim.api.nvim_set_keymap("i", "<C-x><C-o>", "<Cmd>lua vimrc.cmp.omni()<CR>", {noremap = true})
    vim.api.nvim_set_keymap("i", "<C-x>s", "<Cmd>lua vimrc.cmp.spell()<CR>", {noremap = true})
end

return M
