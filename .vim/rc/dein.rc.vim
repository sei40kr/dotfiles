" dein.rc.vim
" author: Seong Yong-ju ( @sei40kr )

let s:config_dir = empty($XDG_CONFIG_HOME) ? expand('~/.config') : $XDG_CONFIG_HOME
let s:cache_dir = empty($XDG_CACHE_HOME) ? expand('~/.cache') : $XDG_CACHE_HOME

let s:dein_cache_dir = s:cache_dir . '/dein'
let s:dein_repo_dir = s:dein_cache_dir . '/repos/github.com/shougo/dein.vim'
if !isdirectory(s:dein_repo_dir)
  call system('git clone --depth 1 https://github.com/Shougo/dein.vim.git ' . shellescape(s:dein_repo_dir))
endif

let g:dein#install_log_filename = expand('~/dein.log')
let g:dein#install_progress_type = 'title'
let g:dein#enable_notification = 1

set runtimepath&
execute 'set runtimepath^=' . s:dein_repo_dir

if !dein#load_state(s:dein_cache_dir)
  finish
endif

call dein#begin(s:dein_cache_dir, expand('<sfile>'))

call dein#load_toml(g:vim_rc_dir . '/dein.toml')
call dein#load_toml(g:vim_rc_dir . '/dein_lazy.toml', { 'lazy': 1 })
call dein#load_toml(g:vim_rc_dir . '/dein_ft.toml', { 'lazy': 1 })

call dein#end()
call dein#save_state()

if !has('vim_starting') && dein#check_install()
  call dein#install()
endif

