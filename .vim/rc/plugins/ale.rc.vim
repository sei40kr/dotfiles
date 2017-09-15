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
    \   'cpp': ['clang', 'clangtidy', 'cppcheck', 'cpplint', 'gcc', 'clang-format'],
    \ }
let g:ale_dockerfile_hadolint_use_docker = 'yes'

nmap <silent> gne <Plug>(ale_next_wrap)
nmap <silent> gep <Plug>(ale_previous_wrap)

