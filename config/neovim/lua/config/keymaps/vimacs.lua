--
--- Navigation

-- backward-char
vim.api.nvim_set_keymap("!", "<C-b>", "<Left>", { noremap = true })

-- forward-char
vim.api.nvim_set_keymap("!", "<C-f>", "<Right>", { noremap = true })

-- previous-line
vim.api.nvim_set_keymap("i", "<C-p>", "<Up>", { noremap = true })

-- next-line
vim.api.nvim_set_keymap("i", "<C-n>", "<Down>", { noremap = true })

-- move-beginning-of-line
vim.api.nvim_set_keymap("!", "<C-a>", "<Home>", { noremap = true })

-- move-end-of-line
vim.api.nvim_set_keymap("!", "<C-e>", "<End>", { noremap = true })

-- backward-sentence
vim.api.nvim_set_keymap("i", "<M-a>", "<C-o>(", { noremap = true })

-- forward-sentence
vim.api.nvim_set_keymap("i", "<M-e>", "<C-o>)", { noremap = true })

-- backward-word
vim.api.nvim_set_keymap("i", "<M-b>", "<C-Left>", { noremap = true })
vim.api.nvim_set_keymap("c", "<M-b>", "<S-Left>", { noremap = true })

-- forward-word
vim.api.nvim_set_keymap("i", "<M-f>", "<C-o>e<Right>", { noremap = true })
vim.api.nvim_set_keymap("c", "<M-f>", "<S-Right>", { noremap = true })

-- scroll-down-command
vim.api.nvim_set_keymap("i", "<M-v>", "<PageUp>", { noremap = true })

-- scroll-up-command
vim.api.nvim_set_keymap("i", "<C-v>", "<PageDown>", { noremap = true })

--
--- Copy & Paste

-- delete-char
vim.api.nvim_set_keymap("!", "<C-d>", "<Del>", { noremap = true })

--
--- Editing

-- TODO M-z zap-to-char

-- kill-region
vim.api.nvim_set_keymap("!", "<M-BS>", "<C-w>", { noremap = true })
vim.api.nvim_set_keymap("i", "<C-BS>", "<C-w>", { noremap = true })

-- kill-line
vim.api.nvim_set_keymap(
    "i",
    "<C-k>",
    '<Cmd>lua require("config.api.vimacs").kill_line()<CR>',
    { noremap = true }
)
vim.api.nvim_set_keymap("c", "<C-k>", "<C-f>d$<C-c><End>", { noremap = true })

-- kill-word
vim.api.nvim_set_keymap(
    "i",
    "<M-d>",
    '<Cmd>lua require("config.api.vimacs").kill_word()<CR>',
    { noremap = true }
)

-- kill-sentence
vim.api.nvim_set_keymap("i", "<M-k>", "<C-o>d)", { noremap = true })
