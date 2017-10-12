" onedark.rc.vim
" author: Seong Yong-ju ( @sei40kr )

let g:onedark_terminal_italics = 1
let g:onedark_color_overrides = {
    \   'red': {'gui': '#E06C75', 'cterm': '1', 'cterm16': '1'},
    \   'dark_red': { 'gui': '#BE5046', 'cterm': '9', 'cterm16': '9' },
    \   'green': {'gui': '#98C379', 'cterm': '2', 'cterm16': '2'},
    \   'yellow': {'gui': '#E5C07B', 'cterm': '3', 'cterm16': '3'},
    \   'dark_yellow': { 'gui': '#D19A66', 'cterm': '11', 'cterm16': '11' },
    \   'blue': {'gui': '#61AFEF', 'cterm': '4', 'cterm16': '4'},
    \   'purple': {'gui': '#C678DD', 'cterm': '5', 'cterm16': '5'},
    \   'cyan': {'gui': '#56B6C2', 'cterm': '6', 'cterm16': '6'},
    \   'vertsplit': {'gui': 'none', 'cterm': 'none', 'cterm16': 'none'},
    \ }

augroup onedark_hooks
  autocmd!
  autocmd ColorScheme onedark
      \ highlight NonText ctermbg=none
      \ | highlight Normal ctermbg=none
      \ | highlight CursorLine guibg=#282c34 ctermbg=235
augroup END

