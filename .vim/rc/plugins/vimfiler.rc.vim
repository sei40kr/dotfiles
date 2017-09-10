" vimfiler.rc.vim
" author: Seong Yong-ju ( @sei40kr )

let g:vimfiler_as_default_explorer = 1
let g:vimfiler_ignore_pattern = ['^\.git$', '^\.DS_Store$']

augroup vimfiler_hooks
  autocmd!
  autocmd FileType vimfiler nnoremap <buffer><expr> v vimfiler#do_switch_action('vsplit')
      \ | nnoremap <buffer><expr> s vimfiler#do_switch_action('split')
      \ | nmap <buffer><expr> <CR> vimfiler#smart_cursor_map("\<Plug>(vimfiler_cd_file)", "\<Plug>(vimfiler_edit_file)")
      \ | nmap <buffer><expr> e vimfiler#smart_cursor_map("\<Plug>(vimfiler_cd_file)", "\<Plug>(vimfiler_edit_file)")
augroup END

call vimfiler#custom#profile('default', 'context', {
    \   'auto_expand': 1,
    \   'buffer_name': 'vimfiler',
    \   'edit_action': 'tabopen',
    \   'explorer': 1,
    \   'find': 1,
    \   'no_quit': 1,
    \   'project': 1,
    \   'safe': 0,
    \   'split': 1,
    \   'toggle': 1,
    \   'winwidth': 30,
    \ })

augroup vimfiler_hooks
  autocmd!
  autocmd FileType vimfiler nmap <buffer> <Tab> <Plug>(vimfiler_switch_to_other_window)
augroup END
