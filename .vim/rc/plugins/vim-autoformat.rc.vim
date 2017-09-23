" vim-autoformat.rc.vim
" author: Seong Yong-ju ( @sei40kr )

let g:autoformat_autoindent = 1
let g:autoformat_remove_trailing_spaces = 1
let g:autoformat_retab = 0

let g:formatdef_buildifier = "'buildifier -mode=fix'"
let g:formatdef_clangformat = "'clang-format -assume-filename='.expand('%:p').' -style=Google'"
let g:formatdef_jsbeautify_html = "'js-beautify --type html'"
let g:formatdef_remark_markdown = "'remark --silent --no-color'"
let g:formatdef_sassconvert_sass = "'sass-convert -F sass -T sass'"
let g:formatdef_stylish_haskell = "'stylish-haskell'"

let g:formatters_bzl = ['buildifier']
let g:formatters_c = []
let g:formatters_cpp = []
let g:formatters_css = []
let g:formatters_haskell = ['stylish_haskell']
let g:formatters_html = ['jsbeautify_html']
let g:formatters_java = ['clangformat']
let g:formatters_javascript = []
let g:formatters_json = []
let g:formatters_markdown = ['remark_markdown']
let g:formatters_python = []
let g:formatters_python3 = []
let g:formatters_ruby = []
let g:formatters_sass = ['sassconvert_sass']
let g:formatters_scss = []
let g:formatters_typescript = []

