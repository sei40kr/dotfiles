vim.cmd("packadd packer.nvim")

local packer = require("packer")

packer.startup({
    function()
        use({
            "@packer_nvim@",
            as = "packer.nvim",
            opt = true,
        })

        use({
            "@impatient_nvim@",
            as = "impatient.nvim",
            commit = "3ea9abedb6941995b05fdad654d9cfd51c38a31f",
        })

        -- Core & Frameworks

        use({
            "folke/tokyonight.nvim",
            as = "tokyonight.nvim",
            opt = false,
            commit = "8223c970677e4d88c9b6b6d81bda23daf11062bb",
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
            "echasnovski/mini.nvim",
            tag = "v0.3.0",
            config = require("config.plugins.mini").config,
        })

        use({
            "kyazdani42/nvim-web-devicons",
            opt = true,
            commit = "09e62319974d7d7ec7e53b974724f7942470ef78",
            config = require("config.plugins.nvim-web-devicons").config,
            module = "nvim-web-devicons",
        })

        use({
            "nvim-lua/plenary.nvim",
            as = "plenary.nvim",
            opt = true,
            commit = "78dde9bc25af3e657eb829058bf179739f7e8e69",
            module = "plenary",
        })

        use({
            "ahmedkhalf/project.nvim",
            opt = true,
            commit = "cef52b8da07648b750d7f1e8fb93f12cb9482988",
            config = require("config.plugins.project_nvim").config,
            module = "project_nvim",
        })

        use({
            "@which_key_nvim@",
            module = "which-key",
        })

        --
        --- Completion

        use({
            "hrsh7th/nvim-cmp",
            as = "nvim-cmp",
            commit = "b5433f901ebffc9e01b82ae13da9a92d49569205",
            requires = {
                {
                    "hrsh7th/cmp-cmdline",
                    commit = "f4beb74e8e036f9532bedbcac0b93c7a55a0f8b0",
                    after = "nvim-cmp",
                },
                {
                    "hrsh7th/cmp-nvim-lsp",
                    as = "cmp-nvim-lsp",
                    commit = "ebdfc204afb87f15ce3d3d3f5df0b8181443b5ba",
                    after = "nvim-cmp",
                },
                {
                    "saadparwaiz1/cmp_luasnip",
                    commit = "b10829736542e7cc9291e60bab134df1273165c9",
                    after = "nvim-cmp",
                },
                {
                    "hrsh7th/cmp-omni",
                    commit = "7a457f0c4f9e0801fee777d955eb841659aa3b84",
                    after = "nvim-cmp",
                },
                {
                    "hrsh7th/cmp-path",
                    commit = "466b6b8270f7ba89abd59f402c73f63c7331ff6e",
                    after = "nvim-cmp",
                },
                {
                    "f3fora/cmp-spell",
                    commit = "5602f1a0de7831f8dad5b0c6db45328fbd539971",
                    after = "nvim-cmp",
                },
                {
                    "onsails/lspkind-nvim",
                    commit = "93e98a0c900327ce7e9be1cbf24aebbe7170e375",
                    module = "lspkind",
                },
            },
            config = require("config.plugins.nvim-cmp").config,
        })

        --
        --- Editor

        use({
            "phaazon/hop.nvim",
            tag = "v1.3.0",
            config = function()
                require("hop").setup()
            end,
            module = "hop",
        })

        use({
            "L3MON4D3/LuaSnip",
            commit = "6b67cb12747225a6412d8263bb97d6d2b8d9366a",
            event = "InsertEnter *",
        })

        use({
            "windwp/nvim-autopairs",
            commit = "38d486a1c47ae2722a78cf569008de0a64f4b153",
            config = require("config.plugins.nvim-autopairs").config,
            event = "InsertEnter *",
        })

        use({
            "mg979/vim-visual-multi",
            tag = "v0.5.8",
            setup = require("config.plugins.vim-visual-multi").setup,
            keys = {
                "<Plug>(VM-Find-Under)",
                "<Plug>(VM-Add-Cursor-Down)",
                "<Plug>(VM-Add-Cursor-Up)",
                "<Plug>(VM-Select-l)",
                "<Plug>(VM-Select-h)",
                "<Plug>(VM-Select-All)",
                "<Plug>(VM-Start-Regex-Search)",
                "<Plug>(VM-Add-Cursor-At-Pos)",
                { "x", "<Plug>(VM-Find-Subword-Under)" },
                { "x", "<Plug>(VM-Visual-Add)" },
                { "x", "<Plug>(VM-Visual-Find)" },
                { "x", "<Plug>(VM-Visual-Cursors)" },
                "<Plug>(VM-Mouse-Cursor)",
                "<Plug>(VM-Mouse-Word)",
                "<Plug>(VM-Mouse-Column)",
            },
        })

        --
        --- UI

        use({
            "akinsho/bufferline.nvim",
            tag = "v1.2.0",
            config = require("config.plugins.bufferline").config,
        })

        use({
            "lewis6991/gitsigns.nvim",
            tag = "v0.4",
            requires = "plenary.nvim",
            config = require("config.plugins.gitsigns").config,
        })

        use({
            "nvim-lualine/lualine.nvim",
            commit = "63f74ac06978cead7cd0cbbb65c80bcda2eede41",
            after = "tokyonight.nvim",
            requires = "tokyonight.nvim",
            config = require("config.plugins.lualine").config,
        })

        use({
            "kyazdani42/nvim-tree.lua",
            commit = "25921aa87a3da31d788870ec2d4e94c723923975",
            requires = "nvim-web-devicons",
            config = require("config.plugins.nvim-tree").config,
            cmd = { "NvimTreeFindFile", "NvimTreeToggle" },
        })

        use({
            "nvim-telescope/telescope.nvim",
            as = "telescope.nvim",
            tag = "nvim-0.5.1",
            requires = {
                "nvim-web-devicons",
                "plenary.nvim",
                {
                    "nvim-telescope/telescope-file-browser.nvim",
                    commit = "c6f5104ff309649ebbaec283bbd1ab54511dd109",
                },
                {
                    "nvim-telescope/telescope-project.nvim",
                    commit = "d317c3cef6917d650d9a638c627b54d3e1173031",
                },
                {
                    "nvim-telescope/telescope-symbols.nvim",
                    commit = "d2d7d6b4298a1f733649526661d872c5e7a75521",
                },
            },
            config = require("config.plugins.telescope_nvim").config,
            cmd = "Telescope",
            module = "telescope",
        })

        use({
            "folke/todo-comments.nvim",
            commit = "98b1ebf198836bdc226c0562b9f906584e6c400e",
            requires = {
                "plenary.nvim",
                "telescope.nvim",
                "trouble.nvim",
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
            "akinsho/toggleterm.nvim",
            commit = "ed812c8eb4d2a297a6a74bd63f3e892179b92776",
            config = require("config.plugins.toggleterm_nvim").config,
            cmd = "ToggleTerm",
            module = "toggleterm",
        })

        use({
            "folke/trouble.nvim",
            commit = "691d490cc4eadc430d226fa7d77aaa84e2e0a125",
            requires = "nvim-web-devicons",
            config = require("config.plugins.trouble_nvim").config,
            cmd = { "Trouble", "TroubleToggle" },
        })

        --
        --- Tools

        use({
            "sindrets/diffview.nvim",
            as = "diffview.nvim",
            commit = "2d1f45282587d565cc4d84112490bc944c0b491d",
            requires = {
                "nvim-web-devicons",
                "plenary.nvim",
            },
            config = require("config.plugins.diffview").config,
            cmd = {
                "DiffviewOpen",
                "DiffviewToggleFiles",
                "DiffviewFileHistory",
            },
            module = "diffview",
        })

        use({
            "TimUntersberger/neogit",
            commit = "c8dd268091ffcbcb673de59c5b37ff26a2eb24ed",
            requires = { "diffview.nvim", "plenary.nvim" },
            config = require("config.plugins.neogit").config,
            cmd = "Neogit",
        })

        use({
            "jose-elias-alvarez/null-ls.nvim",
            commit = "8f7af2ef9d9ff5e331be6725c39ce89a79b36bbc",
            requires = { "plenary.nvim" },
            config = require("config.plugins.null-ls").config,
        })

        use({
            "neovim/nvim-lspconfig",
            tag = "v0.1.3",
            after = "cmp-nvim-lsp",
            requires = {
                "cmp-nvim-lsp",
                {
                    "folke/lua-dev.nvim",
                    commit = "a0ee77789d9948adce64d98700cc90cecaef88d5",
                },
            },
            config = require("config.plugins.nvim-lspconfig").config,
        })

        use({
            "@nvim_treesitter@",
            requires = {
                {
                    "nvim-treesitter/nvim-treesitter-refactor",
                    commit = "0dc8069641226904f9757de786a6ab2273eb73ea",
                },
                {
                    "nvim-treesitter/nvim-treesitter-textobjects",
                    commit = "29c5e9effe53f19f250e3a88d1427b35031bc90d",
                },
                {
                    "windwp/nvim-ts-autotag",
                    commit = "57035b5814f343bc6110676c9ae2eacfcd5340c2",
                },
                {
                    "JoosepAlviste/nvim-ts-context-commentstring",
                    commit = "88343753dbe81c227a1c1fd2c8d764afb8d36269",
                },
                "@nvim_ts_rainbow@",
            },
            config = require("config.plugins.nvim-treesitter").config,
        })

        use({
            "pwntester/octo.nvim",
            commit = "0beb4de71062435ad934caba5728f7f01ae8b969",
            requires = {
                "nvim-web-devicons",
                "plenary.nvim",
                "telescope.nvim",
            },
            config = require("config.plugins.octo").config,
            cmd = "Octo",
        })

        use({
            "tyru/open-browser-github.vim",
            commit = "ac7c034e300f36d591ef234dcd5eb5cd5c07c74f",
            requires = {
                {
                    "tyru/open-browser.vim",
                    commit = "80ec3f2bb0a86ac13c998e2f2c86e16e6d2f20bb",
                },
            },
            cmd = { "OpenGithubFile", "OpenGithubProject" },
        })
    end,
    config = {
        -- Move to lua dir so impatient.nvim can cache it
        compile_path = vim.fn.stdpath("config") .. "/lua/packer_compiled.lua",
    },
})

return packer
