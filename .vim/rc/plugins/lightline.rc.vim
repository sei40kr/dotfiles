" lightline.rc.vim
" author: Seong Yong-ju ( @sei40kr )

function! LLUserMode() abort
  if &ft == 'denite'
    let denite_mode = substitute(denite#get_status_mode(), '-\| ', '', 'g')
    call lightline#link(tolower(denite_mode[0]))
    return denite_mode
  else
    return winwidth(0) > 60 ? lightline#mode() : ''
  endif
endfunction

function! LLUserReadOnly() abort
  return &readonly && &filetype !=# 'help' ? 'RO' : ''
endfunction

function! LLUserALEWarning() abort
  let l:temp = ale#statusline#Count(bufnr(''))
  let l:num_warning = temp.style_warning + temp.warning
  return l:temp.total == 0 ? '' : ' ⚠ ' . num_warning . ' '
endfunction

function! LLUserALEError() abort
  let l:temp = ale#statusline#Count(bufnr(''))
  let l:num_errors = temp.style_error + temp.error
  return l:temp.total == 0 ? '' : ' ⨉ ' . num_errors . ' '
endfunction

let g:lightline = {
    \   'active': {
    \     'right': [['lineinfo', 'ale_error', 'ale_warning'], ['filetype'], ['fileencoding']],
    \   },
    \   'colorscheme': 'onedark',
    \   'component_expand': {
    \     'ale_warning': 'LLUserALEWarning',
    \     'ale_error': 'LLUserALEError',
    \   },
    \   'component_function': {
    \     'mode': 'LLUserMode',
    \     'readonly': 'LLUserReadOnly',
    \   },
    \   'component_type': {
    \     'ale_warning': 'warning',
    \     'ale_error': 'error',
    \   },
    \ }

