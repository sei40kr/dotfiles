" vim-quickrun.rc.vim
" author: Seong Yong-ju ( @sei40kr )

let g:quickrun_no_default_key_mappings = 1
let g:quickrun_config = {
      \   '_': {
      \     'runner': 'vimproc',
      \     'runner/vimproc/updatetime': 100,
      \     'outputter': 'error',
      \     'outputter/buffer/split': ':rightbelow 8sp',
      \     'outputter/error/success': 'buffer',
      \     'outputter/error/error': 'quickfix',
      \     'outputter/quickfix/open_cmd': 'copen',
      \   },
      \ }

nnoremap <silent> <Leader>r :<C-u>QuickRun -mode n<CR>
vnoremap <silent> <Leader>r :<C-u>QuickRun -mode v<CR>

let s:qfclear = {
    \   'name': 'clear_quickfix',
    \   'kind': 'hook',
    \ }

function! s:qfclear.on_normalized(session, context)
  call setqflist([])
endfunction

call quickrun#module#register(s:qfclear, 1)

