local wk = require("which-key")

wk.register(
    {
        ["'"] = {'<Cmd>lua require("telescope.builtin").resume()<CR>', "Resume last search"},
        [":"] = {'<Cmd>lua require("telescope.builtin").commands()<CR>', ":"},
        ["<Space>"] = {'<Cmd>lua require("telescope.builtin").git_files()<CR>', "Find file in project"},
        b = {
            name = "+buffer",
            -- TODO Exclude the buffers of other workspaces
            b = {'<Cmd>lua require("telescope.builtin").buffers()<CR>', "Switch buffer"},
            -- TODO Confirm before killing an unsaved buffer
            d = {"<Cmd>bd<CR>", "Kill buffer"},
            k = {"<Cmd>bd<CR>", "Kill buffer"},
            -- TODO Exclude special buffers
            n = {"<Cmd>BufferLineCycleNext<CR>", "Next buffer"},
            p = {"<Cmd>BufferLineCyclePrev<CR>", "Previous buffer"},
            s = {"<Cmd>w<CR>", "Save buffer"},
            S = {"<Cmd>wa<CR>", "Save all buffers"}
        },
        c = {
            name = "+code",
            d = {"gd", "Jump to definition", noremap = false},
            D = {"gD", "Jump to references", noremap = false},
            k = {"K", "Jump to documentation", noremap = false},
            r = "TS Rename"
        },
        f = {
            name = "+file",
            f = {
                '<Cmd>lua require("telescope").extensions.file_browser.file_browser()<CR>',
                "Find file"
            },
            F = {'<Cmd>lua require("telescope.builtin").find_files()<CR>', "Find file from here"},
            r = {'<Cmd>lua require("telescope.builtin").oldfiles()<CR>', "Recent files"},
            s = {"<Cmd>w<CR>", "Save file"}
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
            a = {'<Cmd>lua require("telescope.builtin").autocommands()<CR>', "autocommands"},
            f = {'<Cmd>lua require("telescope.builtin").filetypes()<CR>', "filetypes"},
            h = {'<Cmd>lua require("telescope.builtin").help_tags()<CR>', "help tags"},
            H = {'<Cmd>lua require("telescope.builtin").highlights()<CR>', "highlights"},
            k = {'<Cmd>lua require("telescope.builtin").keymaps()<CR>', "keymaps"},
            m = {'<Cmd>lua require("telescope.builtin").man_pages()<CR>', "man pages"},
            o = {'<Cmd>lua require("telescope.builtin").vim_options()<CR>', "vim options"}
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
        q = {
            name = "+quit",
            q = {"<Cmd>qa<CR>", "Quit"},
            Q = {"<Cmd>qa!<CR>", "Quit without saving"}
        },
        s = {
            name = "+search",
            j = {'<Cmd>lua require("telescope.builtin").jumplist()<CR>', "Jump list"},
            p = {'<Cmd>lua require("telescope.builtin").live_grep()<CR>', "Search project"},
            r = {'<Cmd>lua require("telescope.builtin").marks()<CR>', "Jump to mark"}
        },
        w = {
            name = "+window",
            ["+"] = {"<C-w>+", "Increase height"},
            ["-"] = {"<C-w>-", "Decrease height"},
            ["<"] = {"<C-w><", "Decrease width"},
            ["="] = {"<C-w>=", "Equally high and wide"},
            [">"] = {"<C-w>>", "Increase width"},
            h = {"<C-w>h", "Go to the left window"},
            j = {"<C-w>j", "Go to the down window"},
            k = {"<C-w>k", "Go to the up window"},
            l = {"<C-w>l", "Go to the right window"},
            q = {"<C-w>q", "Quit a window"},
            s = {"<C-w>s", "Split window"},
            t = {"<C-w>t", "Break out into a new tab"},
            v = {"<C-w>v", "Split window vertically"},
            w = {"<C-w>w", "Switch windows"},
            x = {"<C-w>x", "Swap current with next"}
        }
    },
    {prefix = "<Leader>"}
)
wk.register(
    {
        name = "+git",
        o = {
            name = "+open in browser",
            o = {":OpenGithubFile<CR>", "Browse file"}
        },
        r = {":Gitsigns reset_hunk<CR>", "Revert hunk"},
        s = {":Gitsigns stage_hunk<CR>", "Git stage hunk"}
    },
    {
        mode = "x",
        prefix = "<Leader>g"
    }
)
