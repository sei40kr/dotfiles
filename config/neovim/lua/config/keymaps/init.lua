local wk = require("which-key")

vim.g.mapleader = " "
vim.g.maplocalleader = ","

wk.setup {
    key_labels = {
        ["<space>"] = "SPC",
        ["<cr>"] = "RET",
        ["<tab>"] = "TAB"
    },
    icons = {
        breadcrumb = "",
        separator = ""
    }
}

vim.api.nvim_set_keymap("n", "<Esc>", "<Cmd>nohls<CR>", {})
-- Disable Ex mode
vim.api.nvim_set_keymap("n", "Q", "<Nop>", {noremap = true})

vim.api.nvim_set_keymap("i", "jk", "<Esc>", {noremap = true})

vim.api.nvim_set_keymap("i", "<Tab>", '<Cmd>lua require("config.api.completion").tab_complete()<CR>', {noremap = true})
vim.api.nvim_set_keymap(
    "i",
    "<S-Tab>",
    '<Cmd>lua require("config.api.completion").s_tab_complete()<CR>',
    {noremap = true}
)

vim.api.nvim_set_keymap("t", "jk", "<C-\\><C-n>", {noremap = true})

wk.register({
        ["["] = {
            ["<Space>"] = {'<Cmd>pu! =repeat("\n", v:count1)<CR>', "Insert space above"},
            b = {"<Cmd>BufferLineCyclePrev<CR>", "Previous buffer"},
            c = "Previous comment",
            d = {"<Cmd>Gitsigns prev_hunk<CR>", "Jump to previous hunk"}
        },
        ["]"] = {
            ["<Space>"] = {'<Cmd>pu =repeat("\n", v:count1)<CR>', "Insert space below"},
            b = {"<Cmd>BufferLineCycleNext<CR>", "Next buffer"},
            c = "Next comment",
            d = {"<Cmd>Gitsigns next_hunk<CR>", "Jump to next hunk"}
        }
})

require("config.keymaps.gs")
require("config.keymaps.leader")
require("config.keymaps.vimacs")
