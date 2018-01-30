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

"" SpaceVim/SpaceVim {{{
call SpaceVim#layers#load('autocomplete')
call SpaceVim#layers#load('operator')
call SpaceVim#layers#load('incsearch')
call SpaceVim#layers#load('shell', {
    \ 'default_position': 'bottom',
    \ 'default_height': 30
    \ })
call SpaceVim#layers#load('lsp', {
      \ 'filetypes': ['css', 'haskell', 'html', 'javascript', 'json', 'less', 'php', 'python', 'rust', 'scss'],
      \ 'override_cmd': {
      \ 'css': ['css-languageserver', '--stdio'],
      \ 'javascript': ['flow-language-server', '--stdio'],
      \ 'json': ['json-languageserver', '--stdio'],
      \ 'less': ['css-languageserver', '--stdio'],
      \ 'scss': ['css-languageserver', '--stdio'],
      \ }
      \ })
call SpaceVim#layers#load('lang#c')
call SpaceVim#layers#load('lang#crystal')
call SpaceVim#layers#load('lang#go')
call SpaceVim#layers#load('lang#haskell')
call SpaceVim#layers#load('lang#html')
call SpaceVim#layers#load('lang#java')
call SpaceVim#layers#load('lang#javascript')
call SpaceVim#layers#load('lang#json')
call SpaceVim#layers#load('lang#kotlin')
call SpaceVim#layers#load('lang#lisp')
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

let g:spacevim_max_column = 80
let g:spacevim_windows_leader = 's'
let g:spacevim_unite_leader = 'f'
let g:spacevim_denite_leader = 'F'
let g:spacevim_realtime_leader_guide = 1
let g:spacevim_enable_neomake = 1
let g:spacevim_enable_ale = 0
let g:spacevim_statusline_separator = 'nil'
let g:spacevim_statusline_inactive_separator = 'nil'
let g:spacevim_statusline_unicode_symbols = 1
let g:spacevim_error_symbol = ''
let g:spacevim_warning_symbol = ''
let g:spacevim_info_symbol = ''
let g:spacevim_terminal_cursor_shape = 1
let g:spacevim_vim_help_language = 'ja'
let g:spacevim_language = 'ja_JP.UTF-8'
let g:spacevim_colorscheme = 'onedark'
let g:spacevim_colorscheme_bg = 'dark'
let g:spacevim_colorscheme_default = 'onedark'
let g:spacevim_enable_debug = 1
let g:spacevim_buffer_index_type = 4
let g:spacevim_windows_index_type = 3
let g:spacevim_github_username = 'sei40kr'
let g:spacevim_enable_powerline_fonts = 1
let g:spacevim_project_lint_on_save = 1
let g:spacevim_project_rooter_patterns = ['.git/']
let g:spacevim_project_rooter_automatically = 1
let g:spacevim_lint_on_the_fly = 1
let g:spacevim_enable_vimfiler_welcome = 0
let g:spacevim_enable_vimfiler_gitstatus = 0
let g:spacevim_enable_vimfiler_filetypeicon = 1
let g:spacevim_wildignore .= ',' . join([
      \ '*/tmp/*',
      \ '*.so',
      \ '*.swp',
      \ '*.zip',
      \ '*.pyc',
      \ '*.log',
      \ '*.db',
      \ '*.sqlite',
      \ '*/node_modules/*',
      \ ], ',')
let g:spacevim_disabled_plugins = [
      \ 'chromatica.nvim',
      \ 'fcitx.vim',
      \ 'neco-look',
      \ 'onedark.vim',
      \ 'vim-jsx-pretty',
      \ 'vim-snippets',
      \ ]
let g:spacevim_custom_plugins = [
      \ ['chemzqm/vim-jsx-improve', { 'on_ft': 'javascript' }],
      \ ['ejholmes/vim-forcedotcom'],
      \ ['kana/vim-textobj-help', {
      \ 'depends': 'vim-textobj-user',
      \ 'on_ft': 'help',
      \ }],
      \ ['kana/vim-textobj-jabraces', { 'depends': 'vim-textobj-user' }],
      \ ['raimon49/requirements.txt.vim', { 'on_ft': 'requirements' }],
      \ ['rhysd/committia.vim', { 'on_path': ['COMMIT_EDITMSG', 'MERGE_MSG'] }],
      \ ['rhysd/github-complete.vim', {
      \ 'on_ft': ['gitcommit', 'markdown', 'magit'],
      \ }],
      \ ['sei40kr/SpaceVim-onedark'],
      \ ['thinca/vim-template'],
      \ ['tyru/open-browser-github.vim',  {
      \ 'depends': 'open-browser.vim',
      \ 'on_cmd': ['OpenGithubFile', 'OpenGithubIssue', 'OpenGithubPullReq'],
      \ }],
      \ ]

if has('python3')
  let g:ctrlp_map = ''
  nnoremap <silent> <C-p> :Denite file_rec<CR>
endif
"" }}}

"" airblade/vim-gitgutter {{{
let g:gitgutter_sign_added = '▎'
let g:gitgutter_sign_modified = '▎'
let g:gitgutter_sign_removed = ''
let g:gitgutter_sign_modified_removed = '▎'

let g:gitgutter_map_keys = 0

nmap [c <Plug>GitGutterPrevHunk
nmap ]c <Plug>GitGutterNextHunk
"" }}}

"" blueyed/vim-diminactive {{{
let g:diminactive = 0
"" }}}

"" elzr/vim-json {{{
" conceal by default
let g:vim_json_syntax_conceal = 0
" have warnings by default
let g:vim_json_warnings = 0

augroup SpaceVim_d_lang_json
  autocmd!
  autocmd FileType json setlocal foldmethod=syntax
augroup END
"" }}}

"" jaxbot/github-issues.vim {{{
" When this is set to any value, github-issues will not set Neocomplete and
" Omnicomplete hooks.
let g:github_issues_no_omni = 1
"" }}}

"" joshdick/onedark.vim {{{
let g:onedark_terminal_italics = 1
let g:onedark_color_overrides = {
  \ 'black': { 'gui': '#343742', 'cterm': '235', 'cterm16': '0' },
  \ }

function s:h(group, style) abort
  call onedark#set_highlight(a:group, a:style)
endfunction

function s:setup_onedark() abort
  let s:colors = onedark#GetColors()

  call s:h('Pmenu', { 'fg': s:colors.black, 'bg': s:colors.white })
  call s:h('PmenuSel', { 'fg': s:colors.black, 'bg': s:colors.blue })
  call s:h('PmenuSbar', { 'bg': s:colors.white })
endfunction

augroup SpaceVim_d_colorscheme
  autocmd!
  autocmd ColorScheme onedark call s:setup_onedark()
augroup END
"" }}}

"" junegunn/vim-github-dashboard {{{
let g:github_dashboard = {
      \ 'username': g:spacevim_github_username,
      \ }
"" }}}

"" majutsushi/tagbar {{{
" The number of spaces by which each level is indented. This allows making the
" display more compact or more spacious.
let g:tagbar_indent = 2
" Show the visibility symbols (public/protected/private) to the left of the tag
" name.
let g:tagbar_show_visibility = 1
" Whether line numbers should be shown in the Tagbar window.
let g:tagbar_show_linenumbers = 0
"" }}}

"" neomake/neomake {{{
let g:neomake_ansible_enabled_makers = ['ansiblelint']
let g:neomake_apiblueprint_enabled_makers = ['drafter']
let g:neomake_c_enabled_makers = executable('clang') ? ['clang', 'clangtidy', 'clangcheck'] : ['gcc']
call extend(g:neomake_c_enabled_makers, ['checkpatch', 'cppcheck'])
let g:neomake_cpp_enabled_makers = executable('clang++') ? ['clang', 'clangtidy', 'clangcheck'] : ['gcc']
call add(g:neomake_cpp_enabled_makers, 'cppcheck')
let g:neomake_crystal_enabled_makers = ['crystal']
let g:neomake_css_enabled_makers = ['csslint', 'stylelint']
let g:neomake_go_enabled_makers = ['go', 'gometalinter']
let g:neomake_haskell_enabled_makers = ['ghc-mod', 'hdevtools', 'hlint', 'liquid']
let g:neomake_help_enabled_makers = ['writegood']
let g:neomake_html_enabled_makers = ['tidy', 'htmlhint']
let g:neomake_java_enabled_makers = ['javac']
let g:neomake_javascript_enabled_makers = ['flow', 'eslint_d']
let g:neomake_json_enabled_makers = ['jsonlint']
let g:neomake_jsx_enabled_makers = ['flow', 'eslint_d']
let g:neomake_less_enabled_makers = ['stylelint']
let g:neomake_markdown_enabled_makers = ['mdl']
let g:neomake_perl_enabled_makers = ['perl', 'perlcritic']
let g:neomake_php_enabled_makers = ['php', 'phpmd', 'phpcs', 'phpstan']
let g:neomake_purescript_enabled_makers = ['pulp']
let g:neomake_python_enabled_makers = ['python', 'frosted', 'pylama']
let g:neomake_ruby_enabled_makers = ['mri', 'rubocop', 'rubocop_rails', 'reek', 'rubylint']
let g:neomake_scss_enabled_makers = ['stylelint', 'sasslint']
let g:neomake_sh_enabled_makers = ['sh', 'shellcheck']
let g:neomake_sql_enabled_makers = ['sqlint']
let g:neomake_toml_enabled_makers = ['tomlcheck']
let g:neomake_tsx_enabled_makers = ['tsc', 'tslint']
let g:neomake_typescript_enabled_makers = ['tsc', 'tslint']
let g:neomake_vim_enabled_makers = ['vint']
let g:neomake_vue_enabled_makers = ['eslint_d']
let g:neomake_yaml_enabled_makers = ['yamllint']
let g:neomake_zsh_enabled_makers = ['zsh']
"" }}}

"" othree/javascript-libraries-syntax.vim {{{
let g:used_javascript_libs = ''
"" }}}

"" sbdchd/neoformat {{{
" Run all enabled formatters (by default Neoformat stops after the first
" formatter succeeds)
let g:neoformat_run_all_formatters = 1
" Define custom formatters.
let g:neoformat_enabled_c = ['clangformat']
let g:neoformat_enabled_cpp = ['clangformat']
let g:neoformat_enabled_css = []
let g:neoformat_enabled_go = ['gofmt', 'goimports']
let g:neoformat_enabled_graphql = []
let g:neoformat_enabled_haskell = ['stylishhaskell', 'hindent', 'hfmt']
let g:neoformat_enabled_java = ['googlefmt']
let g:neoformat_enabled_javascript = ['prettiereslint']
let g:neoformat_enabled_json = ['fixjson', 'prettier']
let g:neoformat_enabled_less = []
let g:neoformat_enabled_markdown = []
let g:neoformat_enabled_python = ['autopep8', 'yapf', 'isort']
let g:neoformat_enabled_ruby = ['rubocop']
let g:neoformat_enabled_scss = []
let g:neoformat_enabled_typescript = []
"" }}}

"" pangloss/vim-javascript {{{
augroup SpaceVim_d_lang_javascript
  autocmd!
  autocmd FileType javascript setlocal foldmethod=syntax
augroup END
"" }}}

"" Shougo/deoplete.nvim {{{
" Delay the completion after input in milliseconds.
" Requires |+timers| (Neovim 0.1.5)
let g:deoplete#auto_complete_delay = 150
" If it is "complete", deoplete use |complete()|.
let g:deoplete#complete_method = 'omnifunc'
" If it is 1, file source complete the files from the buffer
" path instead of the current directory.
let g:deoplete#file#enable_buffer_path = 1
" It is a dictionary to decide ignore source names.
" The key is filetype and the value is source names list.
let g:deoplete#ignore_sources = {
      \ '_': ['buffer', 'tag', 'dictionary'],
      \ 'gitcommit': [],
      \ }

set completeopt-=preview
"" }}}

"" Shougo/neosnippet.vim {{{
let g:neosnippet#disable_runtime_snippets = {
      \ 'html': 1,
      \ 'css': 1,
      \ 'scss': 1,
      \ 'sass': 1,
      \ 'less': 1,
      \ }
"" }}}

"" Shougo/vimfiler.vim {{{
" This variable controls vimfiler tree leaf icon.
let g:vimfiler_tree_leaf_icon = ''
" This variable controls vimfiler opened directory tree icon.
let g:vimfiler_tree_opened_icon = '▾'
" This variable controls vimfiler closed directory tree icon.
let g:vimfiler_tree_closed_icon = '▸'
" This variable controls vimfiler file icon.
let g:vimfiler_file_icon = ' '
" This variable controls vimfiler readonly file icon.
let g:vimfiler_readonly_file_icon = ''
" This variable controls vimfiler marked file icon.
let g:vimfiler_marked_file_icon = ''
" This variable controls vimfiler indentation length of tree.
let g:vimfiler_tree_indentation = 2
" Specify the regexp pattern string or list to ignore candidates
" of the source.  This applies on the filenames of candidates.
" It's not case sensitive.
let g:vimfiler_ignore_pattern = [
      \ '^\.DS_Store$',
      \ '^\.git$',
      \ '^\.netrwhist$',
      \ '^\.init\.vim-rplugin\~$',
      \ '\.class$',
      \ '\.rbc$',
      \ '\~$',
      \ '\.pyc$',
      \ '\.db$',
      \ '\.sqlite$',
      \ '^__pycache__$',
      \ ]

if has('mac')
  " It is quick look command for |<Plug>(vimfiler_quick_look)|.
  " Default value is "", so you must set it to use
  " |<Plug>(vimfiler_quick_look)| after installing quick look
  " command.
  let g:vimfiler_quick_look_command = 'qlmanage -p'
endif
"" }}}

"" SpaceVim/deoplete-clang2 {{{
let g:clang2_placeholder_next = ''
let g:clang2_placeholder_prev = ''
"" }}}

"" terryma/vim-multiple-cursors {{{
" If set to 0, then pressing g:multi_cursor_quit_key in Visual mode will not quit and delete all existing cursors. This is useful if you want to press Escape and go back to Normal mode, and still be able to operate on all the cursors.
let g:multi_cursor_exit_from_visual_mode = 0
" If set to 0, then pressing g:multi_cursor_quit_key in Insert mode will not quit and delete all existing cursors. This is useful if you want to press Escape and go back to Normal mode, and still be able to operate on all the cursors.
let g:multi_cursor_exit_from_insert_mode = 0
"" }}}

"" thinca/vim-template {{{
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
"" }}}

"" Yggdroot/indentLine {{{
" Specify whether to enable indentLine plugin by default.
let g:indentLine_enabled = 0
" Specify terminal vim indent line color.
let g:indentLine_color_term = 59
" Specify terminal vim indent line background color.
let g:indentLine_bgcolor_term = 'NONE'
" Specify GUI vim indent line color.
let g:indentLine_color_gui = '#5c6370'
" Specify GUI vim indent line background color.
let g:indentLine_bgcolor_gui = 'NONE'
" Specify a character to show for leading spaces.
let g:indentLine_leadingSpaceChar = '·'
" Specify whether to show leading spaces by default.
let g:indentLine_leadingSpaceEnabled = 1

augroup SpaceVim_d_indentLine
  autocmd!
  autocmd FileType tagbar,vimfiler LeadingSpaceDisable
augroup END
"" }}}

augroup SpaceVim_d_tools
  autocmd!
  autocmd User plugin-template-loaded call s:template_keywords()

  autocmd User MultipleCursorsPre :
  autocmd User MultipleCursorsPost :

  autocmd BufEnter * syntax sync maxlines=200
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
set imdisable
set laststatus=2 tabpagemax=50
set list listchars=eol:¬,tab:»\ ,trail:·
set nobackup noswapfile nowritebackup
set noerrorbells
set noshowmode
set nrformats-=octal
set ruler
set scrolloff=1 sidescrolloff=5
set showcmd
set wildmenu

" vim: set et sw=2 cc=80
