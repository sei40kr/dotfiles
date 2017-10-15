scriptencoding utf-8

" neoformat.vim
" author: Seong Yong-ju

let g:neoformat_run_all_formatters = 1
let g:neoformat_enabled_c = ['clangformat']
let g:neoformat_enabled_cpp = ['clangformat']
let g:neoformat_enabled_crystal = ['crystalformat']
let g:neoformat_enabled_css = []
let g:neoformat_enabled_csv = ['prettydiff']
let g:neoformat_enabled_go = ['goimports', 'gofmt']
let g:neoformat_enabled_graphql = ['prettier']
let g:neoformat_enabled_haskell = ['stylishhaskell']
let g:neoformat_enabled_html = []
let g:neoformat_enabled_java = []
let g:neoformat_enabled_javascript = ['eslint_d']
let g:neoformat_enabled_json = ['prettier']
let g:neoformat_enabled_kotlin = ['ktlint']
let g:neoformat_enabled_less = []
let g:neoformat_enabled_markdown = ['remark']
let g:neoformat_enabled_perl = ['perltidy']
let g:neoformat_enabled_php = []
let g:neoformat_enabled_python = []
let g:neoformat_enabled_ruby = ['rubocop']
let g:neoformat_enabled_rust = ['rustfmt']
let g:neoformat_enabled_scss = []
let g:neoformat_enabled_sh = ['shfmt']
let g:neoformat_enabled_sql = ['sqlformat']
let g:neoformat_enabled_typescript = ['tsfmt']
let g:neoformat_enabled_xml = []

" vim: set et sw=2 cc=80
