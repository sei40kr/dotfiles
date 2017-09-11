" vim-template.rc.vim
" author: Seong Yong-ju ( @sei40kr )

let g:template_basedir = expand('~/.vim/template')
let g:template_files = 'template.*'

let s:template_user = 'Seong Yong-ju'
let s:template_user_id = '@sei40kr'
let s:template_organization = 'TeamSpirit Inc.,'

let s:template_datetime_format = '%Y-%m-%d %H:%M:%S'
let s:template_date_format = '%Y-%m-%d'
let s:template_time_format = '%H:%M:%S'
let s:template_year_format = '%Y'

function! s:template_set_keywords()
  silent! %s/<+FILE_NAME+>/\=expand('%:t')/g

  silent! %s/<+USER+>/\=s:template_user/g
  silent! %s/<+USER_ID+>/\=s:template_user_id/g
  silent! %s/<+ORGANIZATION+>/\=s:template_organization/g

  silent! %s/<+DATETIME+>/\=strftime(s:template_datetime_format)/g
  silent! %s/<+DATE+>/\=strftime(s:template_date_format)/g
  silent! %s/<+TIME+>/\=strftime(s:template_time_format)/g
  silent! %s/<+YEAR+>/\=strftime(s:template_year_format)/g

  if search('<+CURSOR+>')
    execute 'normal! "_da>'
  endif
endfunction

augroup template_hooks
  autocmd!
  autocmd User plugin-template-loaded call s:template_set_keywords()
augroup END

