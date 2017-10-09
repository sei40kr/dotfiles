" ale.rc.vim
" author: Seong Yong-ju ( @sei40kr )

let g:ale_completion_delay = 100
let g:ale_completion_enabled = 1
let g:ale_echo_msg_format = '%severity%: %linter%: %s'
let g:ale_lint_on_save = 1
let g:ale_sign_column_always = 1
let g:ale_sign_error = ' '
let g:ale_sign_info = '●'
let g:ale_sign_warning = ' '
let g:ale_statusline_format = ['  %d', '  %d', '']

let g:ale_fixers = {
    \   'c': ['clang-format'],
    \   'cpp': ['clang-format'],
    \   'css': ['prettier', 'stylelint'],
    \   'javascript': ['prettier', 'eslint'],
    \   'json': ['prettier'],
    \   'python': ['autopep8', 'yapf', 'isort'],
    \   'ruby': ['rubocop'],
    \   'sass': ['stylelint'],
    \   'scss': ['prettier', 'stylelint'],
    \   'typescript': ['prettier'],
    \ }
let g:ale_linters = {
    \   'ansible': ['ansible-lint'],
    \   'awk': ['gawk'],
    \   'bash': ['shellcheck'],
    \   'c': ['clang', 'clang-format'],
    \   'cpp': ['clang', 'clang-format', 'cppcheck', 'cpplint'],
    \   'cmake': ['cmakelint'],
    \   'css': ['stylelint', 'prettier'],
    \   'crystal': ['crystal'],
    \   'go': ['go', 'golint'],
    \   'html': ['htmlhint'],
    \   'javascript': ['eslint', 'prettier'],
    \   'json': ['jsonlint', 'prettier'],
    \   'kotlin': ['kotlinc', 'ktlint'],
    \   'markdown': ['mdl'],
    \   'python': ['autopep8', 'isort', 'yapf'],
    \   'ruby': ['rubocop', 'ruby'],
    \   'rust': ['rls', 'rustc'],
    \   'sass': ['sass-lint', 'stylelint'],
    \   'scss': ['sass-lint', 'stylelint', 'prettier'],
    \   'sh': ['shellcheck'],
    \   'sql': ['sqlint'],
    \   'thrift': ['thrift'],
    \   'typescript': ['tslint', 'tsserver', 'prettier'],
    \   'vim': ['vint'],
    \   'xml': ['xmlint'],
    \   'yaml': ['yamllint'],
    \ }

nmap <silent> gne <Plug>(ale_next_wrap)
nmap <silent> gep <Plug>(ale_previous_wrap)

augroup user-ale-hooks
  autocmd!
  autocmd User ALELint call lightline#update()
augroup END

