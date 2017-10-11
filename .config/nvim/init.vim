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
    \ listchars=tab:░\ ,trail:␣,eol:⏎,extends:→,precedes:←,nbsp:¯
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

if executable('tig')
  command! Tig terminal tig
endif

augroup user_hooks
  autocmd!
  autocmd WinEnter * checktime

  autocmd WinEnter,BufWinEnter * setlocal cursorline
  autocmd WinLeave * setlocal nocursorline
augroup END

nnoremap <silent> <Leader>= :<C-u>Autoformat<CR>

augroup formatter_hooks
  autocmd FileType rust
      \ nnoremap <silent><buffer> <Leader>= :<C-u>RustFmt<CR>
      \ | vnoremap <silent><buffer> <Leader>= :RustFmt<CR>
  autocmd FileType c,cpp,css,javascript,json,python,ruby,sass,scss,typescript
      \ nnoremap <silent><buffer> <Leader>= :<C-u>:ALEFix<CR>
augroup END

augroup vim_script_hook
  autocmd!
  autocmd FileType vim nnoremap <silent><buffer> K :<C-u>exec 'help ' . expand('<cword>')<CR>
augroup END

colorscheme onedark

