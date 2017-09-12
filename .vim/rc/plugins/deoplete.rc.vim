" deoplete.rc.vim
" author: Seong Yong-ju ( @sei40kr )

let g:deoplete#auto_complete_start_length = 1
let g:deoplete#enable_at_startup = 1
let g:deoplete#enable_camel_case = 1
let g:deoplete#enable_fuzzy_completion = 1
let g:deoplete#enable_ignore_case = 1
let g:deoplete#enable_refresh_always = 0
let g:deoplete#enable_smart_case = 1
let g:deoplete#skip_chars = ['(', ')']

inoremap <expr><Tab> pumvisible() ? "\<C-n>" : <SID>check_if_backspace() ? "\<Tab>" : deoplete#manual_complete()
inoremap <expr><S-Tab> pumvisible() ? "\<C-p>" : "\<C-h>"

inoremap <expr><BS> deoplete#smart_close_popup() . "\<C-h>"
inoremap <expr><C-h> deoplete#smart_close_popup() . "\<C-h>"

function! s:check_if_backspace() abort
  let pos = col('.') - 1
  return pos == 0 || getline('.')[pos - 1] =~ '\s'
endfunction

call deoplete#custom#source('_', 'converters', [
    \   'converter_auto_delimiter',
    \   'converter_remove_overlap',
    \   'converter_remove_paren',
    \   'converter_truncate_abbr',
    \   'converter_truncate_menu',
    \ ])

