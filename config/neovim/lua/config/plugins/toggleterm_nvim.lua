local M = {}

function M.config()
    require("toggleterm").setup({
        open_mapping = "<NOP>",
        shade_terminals = false,
        insert_mappings = false,
    })

    vim.cmd([[
augroup toggleterm
autocmd!
autocmd FileType toggleterm nnoremap <buffer> q <Cmd>ToggleTerm<CR>
augroup END
]])
end

return M
