scriptencoding utf-8

let g:mapleader="\<Space>"

" ============ Executable Paths ============
let g:ruby_host_prog = expand('~/.rbenv/shims/ruby')
let g:python_host_prog = expand('~/.pyenv/shims/python2')
let g:python3_host_prog = expand('~/.pyenv/shims/python3')

" ============ Directory Paths ============
let s:cache_dir = (empty($XDG_CACHE_HOME) ? expand('~/.cache') : $XDG_CACHE_HOME)
let s:config_dir = (empty($XDG_CONFIG_HOME) ? expand('~/.config') : $XDG_CONFIG_HOME)
let s:dein_cache_dir = s:cache_dir . '/dein'
let s:dein_repo_dir = s:dein_cache_dir . '/repos/github.com/shougo/dein.vim'
let s:nvim_rc_dir = s:config_dir . '/nvim/rc'
let s:vim_rc_dir = expand('~/.vim/rc')
" Install the plugin manager if it's not installed
if !isdirectory(s:dein_repo_dir)
  call system('git clone --depth 1 https://github.com/Shougo/dein.vim.git ' . shellescape(s:dein_repo_dir))
endif
" Add the repository root to runtime path
set runtimepath&
execute 'set runtimepath^=' . expand('~/.vim')
execute 'set runtimepath^=' . s:dein_repo_dir

execute 'source ' . s:vim_rc_dir . '/common.rc.vim'

let g:dein#install_max_processes = 16
let g:dein#install_progress_type = 'title'
let g:dein#enable_notification = 1

if dein#load_state(s:dein_cache_dir)
  call dein#begin(s:dein_cache_dir)
  call dein#load_toml(s:nvim_rc_dir . '/dein.toml')
  call dein#load_toml(s:nvim_rc_dir . '/dein_lazy.toml', { 'lazy': 1 })
  call dein#load_toml(s:nvim_rc_dir . '/dein_ft.toml', { 'lazy': 1 })
  call dein#end()
  call dein#save_state()
endif

if has('vim_starting') && dein#check_install()
  call dein#install()
endif

set clipboard& clipboard^=unnamedplus,unnamed
set colorcolumn=120
set concealcursor=niv
set conceallevel=2
set cursorline
set cursorline
set expandtab
set fillchars& fillchars+=vert:│
set formatoptions& formatoptions-=ro
set list
set nobackup
set noerrorbells
set noshowmode
set noswapfile
set nowritebackup
set smartindent
set splitbelow
set splitright
set switchbuf=useopen
set t_vb=
set visualbell
set wrap

augroup checktime_hooks
  autocmd!
  autocmd WinEnter * checktime
augroup END

augroup cursorline_hooks
  autocmd!
  autocmd WinEnter,BufWinEnter * setlocal cursorline
  autocmd WinLeave * setlocal nocursorline
augroup END

colorscheme onedark

