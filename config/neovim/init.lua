local config_dir = vim.call('stdpath', 'config')

require('dein')

vim.g.mapleader = '\\<Space>'

vim.cmd('source ' .. config_dir .. '/doom.vim')

vim.opt.ignorecase = true
vim.opt.smartcase = true
vim.opt.wrapscan = true

if vim.call('has', 'termguicolors') == 1 then
    vim.opt.termguicolors = true
end
