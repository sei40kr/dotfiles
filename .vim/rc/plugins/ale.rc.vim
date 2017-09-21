" ale.rc.vim
" author: Seong Yong-ju ( @sei40kr )

let g:ale_lint_on_save = 1

let g:ale_sign_column_always = 1
let g:ale_sign_info = '●'
let g:ale_sign_error = '●'
let g:ale_sign_warning = '●'
let g:ale_echo_msg_error_str = 'E'
let g:ale_echo_msg_format = '[%linter%] %s'
let g:ale_echo_msg_warning_str = 'W'

let g:ale_linters = {
    \   'ansible': ['ansible-lint'],
    \   'awk': ['gawk'],
    \   'bash': ['shellcheck'],
    \   'c': ['clang', 'clang-format'],
    \   'cpp': ['clang', 'clang-format', 'cppcheck', 'cpplint'],
    \   'cmake': ['cmakelint'],
    \   'css': ['prettier', 'stylelint'],
    \   'crystal': ['crystal'],
    \   'go': ['go', 'golint'],
    \   'javascript': ['eslint', 'prettier'],
    \   'json': ['jsonlint', 'prettier'],
    \   'kotlin': ['kotlinc', 'ktlint'],
    \   'markdown': ['mdl'],
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
    \   'zsh': ['shellcheck'],
    \ }
let g:ale_dockerfile_hadolint_use_docker = 'yes'

nmap <silent> gne <Plug>(ale_next_wrap)
nmap <silent> gep <Plug>(ale_previous_wrap)

