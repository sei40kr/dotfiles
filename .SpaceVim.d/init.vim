scriptencoding utf-8

" init.vim
" author: Seong Yong-ju ( @sei40kr )

if has('$RBENV_ROOT')
  let g:ruby_host_prog = $RBENV_ROOT . '/shims/ruby'
endif

if has('$PYENV_ROOT')
  let g:python_host_prog = $PYENV_ROOT . '/shims/python2'
  let g:python3_host_prog = $PYENV_ROOT . '/shims/python3'
endif

let g:mapleader = ';'

let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"

call SpaceVim#layers#load('operator')
call SpaceVim#layers#load('incsearch')
call SpaceVim#layers#load('shell', {
    \ 'default_position': 'bottom',
    \ 'default_height': 30
    \ })
call SpaceVim#layers#load('lsp')
call SpaceVim#layers#load('lang#c')
call SpaceVim#layers#load('lang#crystal')
call SpaceVim#layers#load('lang#go')
call SpaceVim#layers#load('lang#haskell', { 'use_lsp': 1 })
call SpaceVim#layers#load('lang#java')
call SpaceVim#layers#load('lang#javascript', { 'use_lsp': 1 })
call SpaceVim#layers#load('lang#json')
call SpaceVim#layers#load('lang#kotlin')
call SpaceVim#layers#load('lang#perl')
call SpaceVim#layers#load('lang#php')
call SpaceVim#layers#load('lang#python')
call SpaceVim#layers#load('lang#ruby')
call SpaceVim#layers#load('lang#rust')
call SpaceVim#layers#load('lang#sh')
call SpaceVim#layers#load('lang#tmux')
call SpaceVim#layers#load('lang#typescript')
call SpaceVim#layers#load('lang#vim')
call SpaceVim#layers#load('lang#xml')
call SpaceVim#layers#load('tmux')

let g:deoplete#auto_complete_delay = 150
if has('python3')
  let g:ctrlp_map = ''
  nnoremap <silent> <C-p> :Denite file_rec<CR>
endif
let g:clang2_placeholder_next = ''
let g:clang2_placeholder_prev = ''

" SpaceVim configurations
let g:spacevim_max_column = 80
let g:spacevim_windows_leader = 's'
let g:spacevim_unite_leader = 'f'
let g:spacevim_denite_leader = 'F'
let g:spacevim_realtime_leader_guide = 1
let g:spacevim_enable_neomake = 0
let g:spacevim_enable_ale = 1
let g:spacevim_statusline_separator = 'nil'
let g:spacevim_statusline_inactive_separator = 'nil'
let g:spacevim_statusline_unicode_symbols = 0
let g:spacevim_error_symbol = '!'
let g:spacevim_warning_symbol = '?'
let g:spacevim_info_symbol = 'i'
let g:spacevim_terminal_cursor_shape = 1
let g:spacevim_vim_help_language = 'ja'
let g:spacevim_language = 'ja_JP.UTF-8'
let g:spacevim_colorscheme = 'onedark'
let g:spacevim_colorscheme_bg = 'dark'
let g:spacevim_colorscheme_default = 'onedark'
let g:spacevim_enable_debug = 0
let g:spacevim_buffer_index_type = 4
let g:spacevim_windows_index_type = 3
let g:spacevim_github_username = 'sei40kr'
let g:spacevim_enable_powerline_fonts = 1
let g:spacevim_enable_vimfiler_welcome = 0
let g:spacevim_enable_vimfiler_gitstatus = 0

let g:spacevim_disabled_plugins = [
      \ 'vim-snippets',
      \ 'neco-look',
      \ 'fcitx.vim',
      \ ]
let g:spacevim_custom_plugins = [
      \ ['Shougo/context_filetype.vim'],
      \ ['ejholmes/vim-forcedotcom'],
      \ ['heavenshell/vim-jsdoc', { 'on_cmd': 'JsDoc' }],
      \ ['moll/vim-node', { 'on_ft': 'javascript' }],
      \ ['osyo-manga/vim-precious',       { 'depends': 'context_filetype.vim' }],
      \ ['thinca/vim-template'],
      \ ['tyru/open-browser-github.vim',  { 'depends': 'open-browser.vim', 'on_cmd': ['OpenGithubFile', 'OpenGithubIssue', 'OpenGithubPullReq'] }],
      \ ]

" blueyed/vim-diminactive {{{
let g:diminactive_enable_focus = 1
" }}}

" elzr/vim-json {{{
let g:vim_json_syntax_conceal = 0
" }}}

" heavenshell/vim-jsdoc {{{

" Allow prompt for interactive input.
let g:jsdoc_allow_input_prompt = 1

" Prompt for a function description
let g:jsdoc_input_description = 1

" Set value to 1 to turn on detecting underscore starting functions as private convention
let g:jsdoc_underscore_private = 1

" Enable to use ECMAScript6's Shorthand function, Arrow function.
let g:jsdoc_enable_es6 = 1

" }}}

" joshdick/onedark.vim {{{
let g:onedark_terminal_italics = 1
" }}}

" mhinz/vim-signify {{{
let g:gitgutter_sign_added = '▍'
let g:gitgutter_sign_modified = '▍'
let g:gitgutter_sign_removed = ''
let g:gitgutter_sign_modified_removed = '▍'

let g:gitgutter_map_keys = 0

nmap [c <Plug>GitGutterPrevHunk
nmap ]c <Plug>GitGutterNextHunk
" }}}

" osyo-manga/vim-precious {{{
let g:precious_enable_switch_CursorMoved = {
      \ '*': 0,
      \ 'markdown': 1,
      \ 'help': 1,
      \ }
let g:precious_enable_switch_CursorMoved_i = {
      \ '*': 0
      \ }
let g:precious_enable_switch_CursorHold = {
      \ '*': 0
      \ }
" }}}

" othree/javascript-libraries-syntax.vim {{{
let g:used_javascript_libs = ''
" }}}

" sbdchd/neoformat {{{
let g:neoformat_run_all_formatters = 1

let g:neoformat_enabled_c = ['clangformat']
let g:neoformat_enabled_cpp = ['clangformat']
let g:neoformat_enabled_css = ['stylefmt']
let g:neoformat_enabled_go = ['gofmt', 'goimports']
let g:neoformat_enabled_graphql = ['prettier']
let g:neoformat_enabled_haskell = ['stylishhaskell', 'hindent', 'hfmt']
let g:neoformat_enabled_java = ['googlefmt']
let g:neoformat_enabled_javascript = ['prettiereslint']
let g:neoformat_enabled_json = ['prettier']
let g:neoformat_enabled_markdown = ['remark']
let g:neoformat_enabled_python = ['autopep8', 'yapf', 'isort']
let g:neoformat_enabled_ruby = ['rubocop']
let g:neoformat_enabled_scss = ['stylefmt']
let g:neoformat_enabled_typescript = ['prettier', 'tsfmt']
" }}}

" Shougo/context_filetype.vim {{{
let g:context_filetype#search_offset = 300
" }}}

" Shougo/deoplete.nvim {{{
let g:deoplete#ignore_sources = {
      \ 'gitcommit': [],
      \ }
" }}}

" Shougo/neosnippet.vim {{{
let g:neosnippet#disable_runtime_snippets = {
      \ 'html': 1,
      \ 'css': 1,
      \ 'scss': 1,
      \ 'sass': 1,
      \ 'less': 1,
      \ }
" }}}

" Shougo/vimfiler.vim {{{
let g:vimfiler_tree_indentation   = 2
let g:vimfiler_tree_leaf_icon     = ''
let g:vimfiler_tree_opened_icon   = '-'
let g:vimfiler_tree_closed_icon   = '+'
let g:vimfiler_file_icon          = ' '
let g:vimfiler_readonly_file_icon = ' '
let g:vimfiler_marked_file_icon   = '*'

let g:vimfiler_direction = 'topleft'

let g:vimfiler_ignore_pattern = [
      \ '\.class$',
      \ '^\.DS_Store$',
      \ '^\.git$',
      \ '^\.init\.vim-rplugin\~$',
      \ '^\.netrwhist$'
      \ ]

if has('mac')
  let g:vimfiler_quick_look_command = 'qlmanage -p'
endif
" }}}

" thinca/vim-template {{{
let g:template_basedir = expand('~/.SpaceVim.d/template')
let g:template_files = 'template.*'

let s:template_user = 'Seong Yong-ju'
let s:template_user_id = '@sei40kr'
let s:template_organization = 'TeamSpirit Inc.'

let s:template_datetime_format = '%Y-%m-%d %H:%M:%S'
let s:template_date_format = '%Y-%m-%d'
let s:template_time_format = '%H:%M:%S'
let s:template_year_format = '%Y'

function! s:template_keywords() abort
  " vint: -ProhibitCommandRelyOnUser -ProhibitCommandWithUnintendedSideEffect
  silent! %s/<+FILE_NAME+>/\=expand('%:t')/g
  silent! %s/<+FILE_BASE_NAME+>/\=expand('%:t:r')/g

  silent! %s/<+USER+>/\=s:template_user/g
  silent! %s/<+USER_ID+>/\=s:template_user_id/g
  silent! %s/<+ORGANIZATION+>/\=s:template_organization/g

  silent! %s/<+DATETIME+>/\=strftime(s:template_datetime_format)/g
  silent! %s/<+DATE+>/\=strftime(s:template_date_format)/g
  silent! %s/<+TIME+>/\=strftime(s:template_time_format)/g
  silent! %s/<+YEAR+>/\=strftime(s:template_year_format)/g
  " vint: +ProhibitCommandRelyOnUser +ProhibitCommandWithUnintendedSideEffect

  if search('<+CURSOR+>')
    execute 'normal! "_da>'
  endif
endfunction
" }}}

" w0rp/ale {{{
let g:ale_sign_info = g:spacevim_info_symbol
let g:ale_fixers = {
      \ 'c': [],
      \ 'cpp': [],
      \ 'css': [],
      \ 'go': [],
      \ 'haskell': [],
      \ 'javascript': [],
      \ 'json': [],
      \ 'python': [],
      \ 'ruby': [],
      \ 'sass': [],
      \ 'scss': [],
      \ 'typescript': [],
      \ }
let g:ale_linters = {
      \ 'bash': ['shellcheck'],
      \ 'c': ['clang'],
      \ 'cpp': ['clang', 'cpplint', 'cppcheck'],
      \ 'css': ['stylelint'],
      \ 'go': ['go', 'golint'],
      \ 'haskell': ['ghc', 'ghc-mod', 'hlint', 'hfmt'],
      \ 'html': ['htmlhint', 'tidy'],
      \ 'javascript': ['flow', 'eslint'],
      \ 'json': ['jsonlint'],
      \ 'kotlin': ['kotlinc', 'ktlint'],
      \ 'markdown': ['mdl', 'remark_lint'],
      \ 'python': ['pylint', 'pyls'],
      \ 'ruby': ['ruby', 'rubocop', 'rails_best_practices'],
      \ 'rust': ['rls', 'rustc'],
      \ 'sass': ['sass-lint', 'stylelint'],
      \ 'scss': ['scss-lint', 'stylelint'],
      \ 'typescript': ['tsserver', 'tslint'],
      \ 'vim': ['vint'],
      \ }

nnoremap <silent> [q :ALEPrevious<CR>
nnoremap <silent> ]q :ALENext<CR>
" }}}

" Yggdroot/indentLine {{{
let g:indentLine_char = ' '
let g:indentLine_first_char = ' '
let g:indentLine_color_term = 'NONE'
let g:indentLine_bgcolor_term = 236
let g:indentLine_color_gui = 'NONE'
let g:indentLine_bgcolor_gui = '#2c323c'
let g:indentLine_showFirstIndentLevel = 1
" }}}

augroup SpaceVim_d_colorscheme
  autocmd!
  autocmd ColorScheme onedark call onedark#set_highlight('Normal', {
        \ 'fg': onedark#GetColors().white })
augroup END

augroup SpaceVim_d_tools
  autocmd!
  autocmd User plugin-template-loaded call s:template_keywords()

  autocmd InsertEnter * PreciousSwitch
  autocmd InsertLeave * PreciousReset
augroup END

set autochdir
set autoindent expandtab smartindent smarttab
set autoread
set backspace=indent,eol,start
set colorcolumn=80 wrap
set complete& complete-=i
set concealcursor=niv conceallevel=2
set display& display+=lastline
set formatoptions& formatoptions-=ro formatoptions+=j
set history=1000
set hlsearch ignorecase incsearch smartcase wrapscan
set laststatus=2 tabpagemax=50
set list listchars=trail:␣,extends:→,precedes:←
set nobackup noswapfile nowritebackup
set noerrorbells
set noshowmode
set nrformats-=octal
set ruler
set scrolloff=1 sidescrolloff=5
set wildmenu

nnoremap x "_x

" vim: set et sw=2 cc=80
