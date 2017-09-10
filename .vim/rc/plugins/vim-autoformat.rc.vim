" vim-autoformat.rc.vim
" author: Seong Yong-ju ( @sei40kr )

let g:autoformat_autoindent = 0
let g:autoformat_remove_trailing_spaces = 0
let g:autoformat_retab = 0

let g:formatdef_buildifier = "'buildifier -mode=fix'"
let g:formatdef_clangformat = "'clang-format -assume-filename='.expand('%:p').' -style=Google'"
let g:formatdef_jsbeautify_css = "'js-beautify --type css'"
let g:formatdef_jsbeautify_html = "'js-beautify --type html'"
let g:formatdef_jsbeautify_json = "'js-beautify --type json'"
let g:formatdef_remark_markdown = "'remark --silent --no-color'"
let g:formatdef_rubocop = "'rubocop --auto-correct -o /dev/null -s '.bufname('%').' | sed -n 2,\$p'"
let g:formatdef_sassconvert_sass = "'sass-convert -F sass -T sass'"
let g:formatdef_sassconvert_scss = "'sass-convert -F scss -T scss'"
let g:formatdef_stylish_haskell = "'stylish-haskell'"
let g:formatdef_tsfmt = "'tsfmt --stdin'"
let g:formatdef_yapf = "'yapf --style google'"

let g:formatters_bzl = ['buildifier']
let g:formatters_c = ['clangformat']
let g:formatters_cpp = ['clangformat']
let g:formatters_css = ['jsbeautify_css']
let g:formatters_haskell = ['stylish_haskell']
let g:formatters_html = ['jsbeautify_html']
let g:formatters_java = ['clangformat']
let g:formatters_javascript = ['clangformat']
let g:formatters_json = ['jsbeautify_json']
let g:formatters_markdown = ['remark_markdown']
let g:formatters_objc = ['clangformat']
let g:formatters_proto = ['clangformat']
let g:formatters_python = ['yapf']
let g:formatters_python3 = ['yapf']
let g:formatters_ruby = ['rubocop']
let g:formatters_sass = ['sassconvert_sass']
let g:formatters_scss = ['sassconvert_scss']
let g:formatters_typescript = ['tsfmt']

nnoremap <silent> <Leader>= :<C-u>Autoformat<CR>

