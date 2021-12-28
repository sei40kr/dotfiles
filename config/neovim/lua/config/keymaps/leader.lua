local wk = require("which-key")

vim.g.mapleader = " "

wk.setup {
    key_labels = {
        ["<Space>"] = "SPC",
        ["<CR>"] = "RET",
        ["<Tab>"] = "TAB"
    },
    icons = {
        breadcrumb = "",
        separator = ""
    }
}

wk.register(
    {
        ["<Leader>"] = {
            ["'"] = {'<Cmd>lua require("telescope.builtin").resume()<CR>', "Resume last search"},
            [":"] = {'<Cmd>lua require("telescope.builtin").commands()<CR>', ":"},
            ["<Space>"] = {'<Cmd>lua require("telescope.builtin").git_files()<CR>', "Find file in project"},
            b = {
                name = "+buffer",
                B = {'<Cmd>lua require("telescope.builtin").buffers()<CR>', "Switch buffer"}
            },
            c = {
                d = {"gd", "Jump to definition", noremap = false},
                D = {"gD", "Jump to references", noremap = false},
                k = {"K", "Jump to documentation", noremap = false}
            },
            f = {
                name = "+file",
                f = {'<Cmd>lua require("telescope.builtin").file_browser()<CR>', "Find file"},
                F = {'<Cmd>lua require("telescope.builtin").find_files()<CR>', "Find file from here"},
                r = {'<Cmd>lua require("telescope.builtin").oldfiles()<CR>', "Recent files"}
            },
            g = {
                name = "+git",
                ["["] = {"<Cmd>Gitsigns prev_hunk<CR>", "Jump to previous hunk"},
                ["]"] = {"<Cmd>Gitsigns next_hunk<CR>", "Jump to next hunk"},
                b = {'<Cmd>lua require("telescope.builtin").git_branches()<CR>', "Switch branch"},
                g = {"<Cmd>Neogit<CR>", "Neogit status"},
                o = {
                    name = "+open in browser",
                    o = {"<Cmd>OpenGithubFile<CR>", "Browse file"},
                    r = {"<Cmd>OpenGithubProject<CR>", "Browse remote"}
                },
                r = {"<Cmd>Gitsigns reset_hunk<CR>", "Revert hunk"},
                R = {"<Cmd>Gitsigns reset_buffer<CR>", "Revert file"},
                s = {"<Cmd>Gitsigns stage_hunk<CR>", "Git stage hunk"},
                S = {"<Cmd>Gitsigns stage_buffer<CR>", "Git stage file"},
                U = {"<Cmd>Gitsigns reset_buffer_index<CR>", "Git unstage file"}
            },
            h = {
                name = "+help",
                h = {'<Cmd>lua require("telescope.builtin").highlights()<CR>', "highlight"},
                k = {'<Cmd>lua require("telescope.builtin").keymaps()<CR>', "keymap"},
                m = {'<Cmd>lua require("telescope.builtin").man_pages()<CR>', "manpage"},
                o = {'<Cmd>lua require("telescope.builtin").vim_options()<CR>', "vim option"},
                t = {'<Cmd>lua require("telescope.builtin").help_tags()<CR>', "help tag"}
            },
            i = {
                name = "+insert",
                e = {'<Cmd>lua require("telescope.builtin").symbols {sources = {"emoji"}}<CR>', "Emoji"},
                r = {'<Cmd>lua require("telescope.builtin").registers()<CR>', "From register"}
            },
            o = {
                name = "+open",
                p = {"<Cmd>NvimTreeToggle<CR>", "Project sidebar"},
                P = {"<Cmd>NvimTreeFindFile<CR>", "Find file in project sidebar"},
                t = {"<Cmd>ToggleTerm<CR>", "Toggle terminal popup"}
            },
            p = {
                name = "+project",
                p = {
                    '<Cmd>lua require("telescope").extensions.project.project {display_type = "full"}<CR>',
                    "Switch project"
                },
                t = {"<Cmd>TodoTrouble<CR>", "List project todos"}
            },
            s = {
                name = "+search",
                j = {'<Cmd>lua require("telescope.builtin").jumplist()<CR>', "Jump list"},
                p = {'<Cmd>lua require("telescope.builtin").live_grep()<CR>', "Search project"},
                r = {'<Cmd>lua require("telescope.builtin").marks()<CR>', "Jump to mark"}
            }
        },
        ["["] = {
            d = {"<Cmd>Gitsigns prev_hunk<CR>", "Jump to previous hunk"}
        },
        ["]"] = {
            d = {"<Cmd>Gitsigns next_hunk<CR>", "Jump to next hunk"}
        }
    }
)
wk.register(
    {
        name = "+git",
        o = {
            name = "+open in browser",
            o = {"<Cmd>OpenGithubFile<CR>", "Browse file"}
        },
        r = {"<Cmd>Gitsigns reset_hunk<CR>", "Revert hunk"},
        s = {"<Cmd>Gitsigns stage_hunk<CR>", "Git stage hunk"}
    },
    {
        mode = "x",
        prefix = "<Leader>g"
    }
)

vim.api.nvim_set_keymap(
    "n",
    "gs+",
    '<Cmd>lua require("hop").hint_lines_skip_whitespace({ direction = require("hop.hint").HintDirection.AFTER_CURSOR })<CR>',
    {noremap = true}
)
vim.api.nvim_set_keymap(
    "n",
    "gs-",
    '<Cmd>lua require("hop").hint_lines_skip_whitespace({ direction = require("hop.hint").HintDirection.BEFORE_CURSOR })<CR>',
    {noremap = true}
)
vim.api.nvim_set_keymap(
    "n",
    "gsb",
    '<Cmd>lua require("hop").hint_words({ direction = require("hop.hint").HintDirection.BEFORE_CURSOR })<CR>',
    {noremap = true}
)
vim.api.nvim_set_keymap(
    "n",
    "gse",
    '<Cmd>lua require("hop").hint_words({ direction = require("hop.hint").HintDirection.AFTER_CURSOR })<CR>',
    {noremap = true}
)
vim.api.nvim_set_keymap(
    "n",
    "gsf",
    '<Cmd>lua require("hop").hint_char1({ direction = require("hop.hint").HintDirection.AFTER_CURSOR })<CR>',
    {noremap = true}
)
vim.api.nvim_set_keymap(
    "n",
    "gsF",
    '<Cmd>lua require("hop").hint_char1({ direction = require("hop.hint").HintDirection.BEFORE_CURSOR })<CR>',
    {noremap = true}
)
vim.api.nvim_set_keymap(
    "n",
    "gsj",
    '<Cmd>lua require("hop").hint_lines({ direction = require("hop.hint").HintDirection.AFTER_CURSOR })<CR>',
    {noremap = true}
)
vim.api.nvim_set_keymap(
    "n",
    "gsk",
    '<Cmd>lua require("hop").hint_lines({ direction = require("hop.hint").HintDirection.BEFORE_CURSOR })<CR>',
    {noremap = true}
)
vim.api.nvim_set_keymap("n", "gss", '<Cmd>lua require("hop").hint_char2()<CR>', {noremap = true})
