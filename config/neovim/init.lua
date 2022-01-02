require("impatient")

vim.opt.autochdir = true
vim.opt.autoindent = true
vim.opt.autoread = true
vim.opt.backspace = {"indent", "eol", "start"}
vim.opt.clipboard = {"unnamedplus"}
vim.opt.colorcolumn = "80"
vim.opt.completeopt = {"menu", "menuone", "noselect"}
vim.opt.display:append {"lastline"}
vim.opt.encoding = "utf-8"
vim.opt.expandtab = true
vim.opt.fillchars = {diff = "╱"}
vim.opt.formatoptions:append {"j"}
vim.opt.history = 1000
vim.opt.ignorecase = true
vim.opt.laststatus = 2
vim.opt.lazyredraw = true
vim.opt.number = true
vim.opt.relativenumber = true
vim.opt.ruler = true
vim.opt.scrolloff = 1
vim.opt.sidescrolloff = 5
vim.opt.signcolumn = "yes:1"
vim.opt.smartcase = true
vim.opt.splitbelow = true
vim.opt.splitright = true
vim.opt.tabpagemax = 50
vim.opt.wrapscan = true
if vim.call("has", "termguicolors") == 1 then
    vim.opt.termguicolors = true
end

-- Import Doom keymaps
vim.cmd("source " .. vim.fn.stdpath("config") .. "/doom_keymaps.vim")

-- Disable Ex mode
vim.api.nvim_set_keymap("n", "Q", "<Nop>", {noremap = true})

vim.api.nvim_set_keymap("i", "<C-u>", "<C-g>u<C-u>", {noremap = true})
vim.api.nvim_set_keymap("i", "<C-w>", "<C-g>u<C-w>", {noremap = true})

vim.api.nvim_set_keymap("t", "jk", "<C-\\><C-n>", {noremap = true})

vim.cmd [[
augroup init
autocmd!
autocmd FileType help nnoremap <buffer> q <Cmd>bdelete<CR>
augroup END
]]

vim.cmd [[
command! PackerSync lua require('plugins').sync()
command! PackerClean lua require('plugins').clean()
command! PackerCompile lua require('plugins').compile()
]]

_G.vimrc = {}
require("packer_compiled")
require("config.keymaps")
