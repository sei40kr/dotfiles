scriptencoding utf-8

let g:mapleader="\<Space>"

" ============ Directory Paths ============
let s:cache_dir = (empty($XDG_CACHE_HOME) ? expand('~/.cache') : $XDG_CACHE_HOME)
let s:config_dir = (empty($XDG_CONFIG_HOME) ? expand('~/.config') : $XDG_CONFIG_HOME)
let s:dein_cache_dir = s:cache_dir . '/dein'
let s:dein_repo_dir = s:cache_dir . '/repos/github.com/Shougo/dein.vim'
let s:nvim_rc_dir = s:config_dir . '/nvim/rc'
let s:vim_rc_dir = expand('~/.vim/rc')
" Install the plugin manager if it's not installed
if !isdirectory(s:dein_repo_dir)
  call system('git clone --depth 1 https://github.com/Shougo/dein.vim.git ' . shellescape(s:dein_repo_dir))
endif
" Add the repository root to runtime path
set runtimepath&
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

" Integrate with system clipboard
set clipboard& clipboard^=unnamedplus,unnamed
set cmdheight=2
set colorcolumn=120 wrap
set cursorline
set expandtab smartindent
set fillchars& fillchars+=vert:│
set formatoptions& formatoptions-=ro
set list
" Disable back-up files
set nobackup nowritebackup noswapfile
" Disable beeps and its visual effects
set noerrorbells visualbell t_vb=
set splitbelow splitright
set switchbuf=useopen

augroup user_hooks
  autocmd!
  autocmd WinEnter * checktime

  autocmd InsertEnter * set norelativenumber number
  autocmd InsertLeave * set nonumber relativenumber

  autocmd FileType help,qf nnoremap <buffer> q :<C-u>q<CR>
augroup END

colorscheme dracula

