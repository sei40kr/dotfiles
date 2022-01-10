vim.cmd("packadd packer.nvim")

local packer = require("packer")

packer.startup({
    function()
        use({
            "@impatient_nvim@",
            as = "impatient.nvim",
        })

        use({
            "@packer_nvim@",
            as = "packer.nvim",
            opt = true,
        })

        -- Core & Frameworks

        use({
            "@tokyonight_nvim@",
            as = "tokyonight.nvim",
            opt = false,
            config = function()
                vim.cmd("colorscheme tokyonight")
            end,
            setup = function()
                vim.g.tokyonight_style = "night"
                vim.g.tokyonight_italic_keywords = false
                vim.g.tokyonight_transparent = vim.g.GuiLoaded ~= nil
                vim.g.tokyonight_transparent_sidebar = vim.g.GuiLoaded ~= nil
                vim.g.tokyonight_lualine_bold = true
            end,
        })

        use({
            "@nvim_web_devicons@",
            as = "nvim-web-devicons",
            opt = true,
            config = require("config.plugins.nvim-web-devicons").config,
            module = "nvim-web-devicons",
        })

        use({
            "@plenary_nvim@",
            as = "plenary.nvim",
            opt = true,
            module = "plenary",
        })

        use({
            "@project_nvim@",
            opt = true,
            config = require("config.plugins.project_nvim").config,
            module = "project_nvim",
        })

        use({
            "@vim_repeat@",
            as = "vim-repeat",
            opt = true,
            config = require("config.plugins.vim-repeat").setup,
            keys = {
                "<Plug>(RepeatDot)",
                "<Plug>(RepeatUndo)",
                "<Plug>(RepeatUndoLine)",
                "<Plug>(RepeatRedo)",
            },
        })

        use({
            "@which_key_nvim@",
            as = "which-key.nvim",
        })

        --
        --- Completion

        use({
            "@nvim_cmp@",
            as = "cmp-nvim-lua",
            requires = {
                { "@cmp_cmdline@", after = "cmp-nvim-lua" },
                {
                    "@cmp_nvim_lsp@",
                    as = "cmp-nvim-lsp",
                    after = "cmp-nvim-lua",
                },
                { "@cmp_luasnip@", after = "cmp-nvim-lua" },
                { "@cmp_omni@", after = "cmp-nvim-lua" },
                { "@cmp_path@", after = "cmp-nvim-lua" },
                { "@cmp_spell@", after = "cmp-nvim-lua" },
                { "@lspkind_nvim@", module = "lspkind" },
            },
            config = require("config.plugins.nvim-cmp").config,
        })

        --
        --- Editor

        use({
            "@clever_f_vim@",
            setup = require("config.plugins.clever-f_vim").setup,
        })

        use({
            "@hop_nvim@",
            config = function()
                require("hop").setup()
            end,
            module = "hop",
        })

        use({
            "@luasnip@",
            event = "InsertEnter *",
        })

        use({
            "@nvim_autopairs@",
            config = require("config.plugins.nvim-autopairs").config,
            event = "InsertEnter *",
        })

        use({
            "@vim_commentary@",
            requires = "vim-repeat",
            setup = require("config.plugins.vim-commentary").setup,
            keys = {
                "<Plug>Commentary",
                { "x", "<Plug>Commentary" },
                { "o", "<Plug>Commentary" },
                "<Plug>CommentaryLine",
                "<Plug>ChangeCommentary",
            },
        })

        use({
            "@vim_surround@",
            requires = "vim-repeat",
            setup = require("config.plugins.vim-surround").setup,
            keys = {
                "<Plug>Dsurround",
                "<Plug>Csurround",
                "<Plug>CSurround",
                "<Plug>Ysurround",
                "<Plug>YSurround",
                "<Plug>Yssurround",
                "<Plug>YSsurround",
                "<Plug>YSsurround",
                { "x", "<Plug>VSurround" },
                { "x", "<Plug>VgSurround" },
            },
        })

        --
        --- UI

        use({
            "@bufferline_nvim@",
            config = require("config.plugins.bufferline_nvim").config,
        })

        use({
            "@gitsigns_nvim@",
            requires = "plenary.nvim",
            config = require("config.plugins.gitsigns_nvim").config,
        })

        use({
            "@lualine_nvim@",
            after = "tokyonight.nvim",
            requires = "tokyonight.nvim",
            config = require("config.plugins.lualine_nvim").config,
        })

        use({
            "@nvim_tree_lua@",
            requires = "nvim-web-devicons",
            config = require("config.plugins.nvim-tree").config,
            cmd = { "NvimTreeFindFile", "NvimTreeToggle" },
        })

        use({
            "@telescope_nvim@",
            as = "telescope.nvim",
            requires = {
                "nvim-web-devicons",
                "plenary.nvim",
                "@telescope_file_browser_nvim@",
                "@telescope_project_nvim@",
                "@telescope_symbols_nvim@",
            },
            config = require("config.plugins.telescope_nvim").config,
            cmd = "Telescope",
            module = "telescope",
        })

        use({
            "@todo_comments_nvim@",
            requires = {
                "plenary.nvim",
                "@telescope_nvim@",
                "@trouble_nvim@",
            },
            config = require("config.plugins.todo-comments_nvim").config,
            cmd = {
                "TodoQuickFix",
                "TodoLocList",
                "TodoTrouble",
                "TodoTelescope",
            },
        })

        use({
            "@toggleterm_nvim@",
            config = require("config.plugins.toggleterm_nvim").config,
            cmd = "ToggleTerm",
            module = "toggleterm",
        })

        use({
            "@trouble_nvim@",
            requires = "nvim-web-devicons",
            config = require("config.plugins.trouble_nvim").config,
            cmd = { "Trouble", "TroubleToggle" },
        })

        --
        --- Tools

        use({
            "@diffview_nvim@",
            as = "diffview.nvim",
            requires = { "nvim-web-devicons", "plenary.nvim" },
            config = require("config.plugins.diffview_nvim").config,
            cmd = {
                "DiffviewOpen",
                "DiffviewToggleFiles",
                "DiffviewFileHistory",
            },
            module = "diffview",
        })

        use({
            "@neogit@",
            requires = { "diffview.nvim", "plenary.nvim" },
            config = require("config.plugins.neogit").config,
            cmd = "Neogit",
        })

        use({
            "@null_ls_nvim@",
            requires = { "plenary.nvim" },
            config = require("config.plugins.null-ls-nvim").config,
        })

        use({
            "@nvim_lspconfig@",
            after = "cmp-nvim-lsp",
            requires = { "cmp-nvim-lsp", "@lua_dev_nvim@" },
            config = require("config.plugins.nvim-lspconfig").config,
        })

        use({
            "@nvim_treesitter@",
            requires = {
                "@nvim_treesitter_refactor@",
                "@nvim_treesitter_textobjects@",
                "@nvim_ts_autotag@",
                "@nvim_ts_context_commentstring@",
                "@nvim_ts_rainbow@",
            },
            config = require("config.plugins.nvim-treesitter").config,
        })

        use({
            "@octo_nvim@",
            requires = {
                "nvim-web-devicons",
                "plenary.nvim",
                "telescope.nvim",
            },
            config = require("config.plugins.octo_nvim").config,
            cmd = "Octo",
        })

        use({
            "@open_browser_github_vim@",
            requires = "@open_browser_vim@",
            cmd = { "OpenGithubFile", "OpenGithubProject" },
        })
    end,
    config = {
        -- Move to lua dir so impatient.nvim can cache it
        compile_path = vim.fn.stdpath("config") .. "/lua/packer_compiled.lua",
    },
})

return packer
