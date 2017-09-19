" lightline.rc.vim
" author: Seong Yong-ju ( @sei40kr )

function! LLUserMode()
  if &ft == 'denite'
    let denite_mode = substitute(denite#get_status_mode(), '-\\| ', '', 'g')
    call lightline#link(tolower(denite_mode[0]))
    return denite_mode
  else
    return winwidth('.') > 60 ? lightline#mode() : ''
  endif
endfunction

let g:lightline = {
    \   'colorscheme': 'onedark',
    \   'component_function': {
    \     'mode': 'LLUserMode',
    \   },
    \ }

