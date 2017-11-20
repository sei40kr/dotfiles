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

call SpaceVim#layers#load('operator')
call SpaceVim#layers#load('incsearch')
call SpaceVim#layers#load('tags')
call SpaceVim#layers#load('shell', {
    \ 'default_position': 'bottom',
    \ 'default_height': 30
    \ })
call SpaceVim#layers#load('lsp')
call SpaceVim#layers#load('lang#c')
call SpaceVim#layers#load('lang#crystal')
call SpaceVim#layers#load('lang#go')
call SpaceVim#layers#load('lang#haskell')
call SpaceVim#layers#load('lang#java')
call SpaceVim#layers#load('lang#javascript', { 'autofix': 1 })
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
let g:spacevim_max_column                    = 80
let g:spacevim_windows_leader                = 's'
let g:spacevim_unite_leader                  = 'f'
let g:spacevim_denite_leader                 = 'F'
let g:spacevim_realtime_leader_guide         = 1
let g:spacevim_enable_neomake                = 0
let g:spacevim_enable_ale                    = 1
let g:spacevim_statusline_separator          = 'nil'
let g:spacevim_statusline_inactive_separator = 'nil'
let g:spacevim_error_symbol                  = ''
let g:spacevim_warning_symbol                = ''
let g:spacevim_info_symbol                   = ''
let g:spacevim_terminal_cursor_shape         = 1
let g:spacevim_vim_help_language             = 'ja'
let g:spacevim_language                      = 'ja_JP.UTF-8'
let g:spacevim_colorscheme                   = 'onedark'
let g:spacevim_colorscheme_bg                = 'dark'
let g:spacevim_colorscheme_default           = 'onedark'
let g:spacevim_enable_debug                  = 0
let g:spacevim_buffer_index_type             = 4
let g:spacevim_windows_index_type            = 3
let g:spacevim_github_username               = 'sei40kr'
let g:spacevim_enable_powerline_fonts        = 1
let g:spacevim_enable_vimfiler_welcome       = 0
let g:spacevim_enable_vimfiler_gitstatus     = 0

let g:spacevim_disabled_plugins = [
      \ 'vim-snippets',
      \ 'neco-look',
      \ 'vim-diminactive',
      \ 'fcitx.vim',
      \ ]
let g:spacevim_custom_plugins = [
      \ ['Shougo/context_filetype.vim'],
      \ ['ejholmes/vim-forcedotcom'],
      \ ['osyo-manga/vim-precious',       { 'depends': 'context_filetype.vim' }],
      \ ['thinca/vim-template'],
      \ ['tyru/open-browser-github.vim',  { 'depends': 'open-browser.vim', 'on_cmd': ['OpenGithubFile', 'OpenGithubIssue', 'OpenGithubPullReq'] }],
      \ ]

" easymotion/vim-easymotion {{{
let g:EasyMotion_keys = 'sdghklwertyuioxcvbnmfj'
" }}}

" elzr/vim-json {{{
let g:vim_json_syntax_conceal = 0
" }}}

" itchyny/calendar.vim {{{
let g:calendar_first_day = 'monday'
let g:calendar_task = 1
let g:calendar_event_start_time = 0
let g:calendar_google_task = 1
let g:calendar_google_calendar = 1
" }}}

" joshdick/onedark.vim {{{
let g:onedark_terminal_italics = 1
" }}}

" mhinz/vim-signify {{{
let g:gitgutter_sign_added = '▌'
let g:gitgutter_sign_modified = '▌'
let g:gitgutter_sign_removed = ''
let g:gitgutter_sign_modified_removed = '▌'

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
let g:used_javascript_libs = 'jquery,underscore,react,requirejs,d3'
" }}}

" sbdchd/neoformat {{{
let g:neoformat_run_all_formatters = 1

let g:neoformat_enabled_c          = []
let g:neoformat_enabled_cpp        = []
let g:neoformat_enabled_crystal    = ['crystalformat']
let g:neoformat_enabled_css        = []
let g:neoformat_enabled_csv        = []
let g:neoformat_enabled_go         = ['goimports']
let g:neoformat_enabled_graphql    = []
let g:neoformat_enabled_haskell    = ['stylishhaskell', 'hindent']
let g:neoformat_enabled_html       = ['tidy']
let g:neoformat_enabled_java       = ['googlefmt']
let g:neoformat_enabled_javascript = []
let g:neoformat_enabled_json       = []
let g:neoformat_enabled_kotlin     = ['ktlint']
let g:neoformat_enabled_less       = []
let g:neoformat_enabled_markdown   = ['remark']
let g:neoformat_enabled_perl       = ['perltidy']
let g:neoformat_enabled_php        = ['phpbeautifier', 'phpcsfixer']
let g:neoformat_enabled_python     = []
let g:neoformat_enabled_ruby       = []
let g:neoformat_enabled_rust       = ['rustfmt']
let g:neoformat_enabled_sass       = []
let g:neoformat_enabled_scss       = []
let g:neoformat_enabled_sh         = ['shfmt']
let g:neoformat_enabled_sql        = ['sqlformat']
let g:neoformat_enabled_typescript = ['tsfmt']
let g:neoformat_enabled_xhtml      = ['tidy']
let g:neoformat_enabled_xml        = ['tidy']
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
let g:vimfiler_tree_indentation   = 1
let g:vimfiler_tree_leaf_icon     = ''
let g:vimfiler_tree_opened_icon   = '  '
let g:vimfiler_tree_closed_icon   = '  '
let g:vimfiler_file_icon          = '   '
let g:vimfiler_readonly_file_icon = '  '
let g:vimfiler_marked_file_icon   = '  '

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

" SpaceVim/LanguageClient-neovim {{{
let g:LanguageClient_serverCommands = {
      \ 'apex': ['java',
      \ '-cp', expand('~/.SpaceVim.d/libexec/apex-jorje-lsp.jar'),
      \ '-Ddebug.internal.errors=true',
      \ '-Ddebug.semantic.errors=false',
      \ 'apex.jorje.lsp.ApexLanguageServerLauncher'],
      \ 'css':        ['css-language-server', '--stdio'],
      \ 'dockerfile': ['docker-langserver', '--stdio'],
      \ 'go':         ['go-langserver'],
      \ 'haskell':    ['hie', '--lsp'],
      \ 'javascript': ['javascript-typescript-stdio'],
      \ 'less':       ['css-language-server', '--stdio'],
      \ 'python':     ['pyls'],
      \ 'sass':       ['css-language-server', '--stdio'],
      \ 'scss':       ['css-language-server', '--stdio'],
      \ 'typescript': ['javascript-typescript-stdio'],
      \ }
" }}}

" ternjs/tern_for_vim {{{
let g:tern_show_argument_hints = 'on_hold'
let g:tern_show_signature_in_pum = 1
" }}}

" thinca/vim-template {{{
let g:template_basedir = expand('~/.SpaceVim.d/template')
let g:template_files = 'template.*'

let s:template_user         = 'Seong Yong-ju'
let s:template_user_id      = '@sei40kr'
let s:template_organization = 'TeamSpirit Inc.'

let s:template_datetime_format = '%Y-%m-%d %H:%M:%S'
let s:template_date_format     = '%Y-%m-%d'
let s:template_time_format     = '%H:%M:%S'
let s:template_year_format     = '%Y'

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
      \ 'c'          : ['clang-format'],
      \ 'cpp'        : ['clang-format'],
      \ 'css'        : ['prettier', 'stylelint'],
      \ 'go'         : ['gofmt'],
      \ 'haskell'    : ['hfmt'],
      \ 'javascript' : ['prettier', 'eslint'],
      \ 'json'       : ['prettier'],
      \ 'python'     : ['autopep8', 'yapf', 'isort'],
      \ 'ruby'       : ['rubocop'],
      \ 'sass'       : ['prettier', 'stylelint'],
      \ 'scss'       : ['prettier', 'stylelint'],
      \ 'typescript' : ['prettier'],
      \ }
let g:ale_linters = {
      \ 'ansible'    : ['ansible-lint'],
      \ 'awk'        : ['gawk'],
      \ 'bash'       : ['shellcheck'],
      \ 'c'          : ['clang', 'clang-format'],
      \ 'cpp'        : ['clang', 'clang-format', 'cppcheck', 'cpplint'],
      \ 'cmake'      : ['cmakelint'],
      \ 'css'        : ['stylelint'],
      \ 'crystal'    : ['crystal'],
      \ 'dockerfile' : ['hadolint'],
      \ 'go'         : ['go', 'golint'],
      \ 'haskell'    : ['ghc', 'ghc-mod', 'hlint', 'hdevtools', 'hfmt'],
      \ 'html'       : ['htmlhint'],
      \ 'javascript' : ['eslint'],
      \ 'json'       : ['jsonlint'],
      \ 'kotlin'     : ['kotlinc', 'ktlint'],
      \ 'markdown'   : ['mdl', 'remark-lint'],
      \ 'python'     : ['autopep8', 'isort', 'yapf'],
      \ 'ruby'       : ['rubocop', 'ruby'],
      \ 'rust'       : ['rls', 'rustc'],
      \ 'sass'       : ['sass-lint', 'stylelint'],
      \ 'scss'       : ['sass-lint', 'stylelint'],
      \ 'sh'         : ['shellcheck'],
      \ 'sql'        : ['sqlint'],
      \ 'thrift'     : ['thrift'],
      \ 'typescript' : ['tslint', 'tsserver'],
      \ 'vim'        : ['vint'],
      \ 'xml'        : ['xmlint'],
      \ 'yaml'       : ['yamllint'],
      \ }
let g:ale_javascript_stylelint_executable = 'stylelint_d'
let g:ale_javascript_stylelint_use_global = 1

nnoremap <silent> [q :ALEPrevious<CR>
nnoremap <silent> ]q :ALENext<CR>
" }}}

" Yggdroot/indentLine {{{
let g:indentLine_char = '▏'
let g:indentLine_color_term = 236
let g:indentLine_bgcolor_term = 'NONE'
let g:indentLine_color_gui = '#2c323c'
let g:indentLine_bgcolor_gui = 'NONE'
let g:indentLine_fileTypeExclude = []
let g:indentLine_setConceal = 0
" }}}

augroup SpaceVim_d_colorscheme
  autocmd!
  autocmd ColorScheme onedark call onedark#set_highlight('Normal', { 'fg': onedark#GetColors().white })
augroup END

augroup SpaceVim_d_tools
  autocmd!
  autocmd User plugin-template-loaded call s:template_keywords()

  autocmd InsertEnter * PreciousSwitch
  autocmd InsertLeave * PreciousReset

  autocmd FocusGained * set cursorline
  autocmd FocusLost * set nocursorline
augroup END

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
