" lightline.rc.vim
" author: Seong Yong-ju ( @sei40kr )

function! LLUserMode() abort
  if &ft == 'denite'
    let l:mode = substitute(denite#get_status_mode(), '-\| ', '', 'g')
    call lightline#link(tolower(l:mode[0]))
    return l:mode
  endif
  return winwidth(0) > 60 ? lightline#mode() : ''
endfunction

function! LLUserReadOnly() abort
  return &filetype ==# 'help' ? '' : (&readonly ? 'RO' : '')
endfunction

function! LLUserFugitive() abort
  return winwidth(0) >= 100 ? gitbranch#name() : ''
endfunction

function! LLUserModified() abort
  return &filetype ==# 'help' ? '' : (&modified ? '' : (&modifiable ? '' : '-'))
endfunction

" TODO Refactor these functions
function! LLUserALEError() abort
  if &filetype ==# 'denite'
    return ''
  endif
  let l:counts = ale#statusline#Count(bufnr('%'))
  let l:errors = counts.style_error + counts.error
  return l:errors ? printf(g:ale_statusline_format[0], l:errors) : ''
endfunction
function! LLUserALEWarning() abort
  if &filetype ==# 'denite'
    return ''
  endif
  let l:counts = ale#statusline#Count(bufnr('%'))
  let l:warnings = counts.style_warning + counts.warning
  return l:warnings ? printf(g:ale_statusline_format[1], l:warnings) : ''
endfunction
function! LLUserALEOK() abort
  if &filetype ==# 'denite'
    return ''
  endif
  let l:counts = ale#statusline#Count(bufnr('%'))
  return l:counts.total ? '' : g:ale_statusline_format[2]
endfunction

let g:lightline = {
    \   'active': {
    \     'left': [['mode', 'paste'], ['fugitive'], ['filepath'], ['filename']],
    \     'right': [
    \       ['lineinfo'],
    \       ['percent'],
    \       ['ale_error', 'ale_warning', 'ale_ok', 'fileencoding', 'filetype'],
    \     ],
    \   },
    \   'inactive': {
    \     'left': [['filepath', 'filename']],
    \     'right': [['lineinfo'], ['percent']],
    \   },
    \   'colorscheme': 'onedark',
    \   'component_expand': {
    \     'ale_error': 'LLUserALEError',
    \     'ale_warning': 'LLUserALEWarning',
    \     'ale_ok': 'LLUserALEOK',
    \   },
    \   'component_function': {
    \     'fugitive': 'LLUserFugitive',
    \     'mode': 'LLUserMode',
    \     'modified': 'LLUserModified',
    \     'readonly': 'LLUserReadOnly',
    \   },
    \   'component_type': {
    \     'ale_error': 'error',
    \     'ale_warning': 'warning',
    \     'ale_ok': 'ok',
    \   },
    \ }

