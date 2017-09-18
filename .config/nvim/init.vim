scriptencoding utf-8

let g:mapleader="\<Space>"

let g:ruby_host_prog = expand('~/.rbenv/shims/ruby')
let g:python_host_prog = expand('~/.pyenv/shims/python2')
let g:python3_host_prog = expand('~/.pyenv/shims/python3')

let g:vim_rc_dir = expand('~/.vim/rc')

set runtimepath&
execute 'set runtimepath^=' . expand('~/.vim')

execute 'source ' . g:vim_rc_dir . '/common.rc.vim'
execute 'source ' . g:vim_rc_dir . '/dein.rc.vim'

syntax enable
filetype plugin indent on

set clipboard& clipboard^=unnamedplus,unnamed
    \ colorcolumn=120
    \ concealcursor=niv
    \ conceallevel=2
    \ cursorline
    \ expandtab
    \ fillchars& fillchars+=vert:│
    \ formatoptions& formatoptions-=ro
    \ list
    \ nobackup
    \ noerrorbells
    \ noshowmode
    \ noswapfile
    \ nowritebackup
    \ smartindent
    \ splitbelow
    \ splitright
    \ switchbuf=useopen
    \ t_vb=
    \ visualbell
    \ wrap

augroup user_hooks
  autocmd!
  autocmd WinEnter * checktime

  autocmd WinEnter,BufWinEnter * setlocal cursorline
  autocmd WinLeave * setlocal nocursorline
augroup END

colorscheme onedark

