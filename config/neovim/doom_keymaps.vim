imap jk <Esc>

" Insert newline above
nnoremap [<Space> O<Esc>j
" Insert newline below
nnoremap ]<Space> o<Esc>k


"
" +buffer

" Kill buffer
nnoremap <Leader>bd :<C-u>bdelete<CR>
" Kill buffer
nnoremap <Leader>bk :<C-u>bdelete<CR>
" Next buffer
nnoremap <Leader>bn :<C-u>bn<CR>
" Previous buffer
nnoremap <Leader>bp :<C-u>bp<CR>
" Save buffer
nnoremap <Leader>bs :<C-u>w<CR>
vnoremap <Leader>bs :<C-u>w<CR>
" Save all buffers
nnoremap <Leader>bS :<C-u>wa<CR>
vnoremap <Leader>bS :<C-u>wa<CR>


"
" +code

" Jump to definition
nnoremap <Leader>cd gd
vnoremap <Leader>cd gd


"
" +file

" Save file
nnoremap <Leader>fs :<C-u>w<CR>
vnoremap <Leader>fs :<C-u>w<CR>


"
" +window

nnoremap <Leader>wc <C-w>c
nnoremap <Leader>wd <C-w>c
vnoremap <Leader>wc <C-w>c
vnoremap <Leader>wd <C-w>c
nnoremap <Leader>wh <C-w>h
vnoremap <Leader>wh <C-w>h
nnoremap <Leader>wj <C-w>j
vnoremap <Leader>wj <C-w>j
nnoremap <Leader>wk <C-w>k
vnoremap <Leader>wk <C-w>k
nnoremap <Leader>wl <C-w>l
vnoremap <Leader>wl <C-w>l
nnoremap <Leader>ws <C-w>s
vnoremap <Leader>ws <C-w>s
nnoremap <Leader>wv <C-w>v
vnoremap <Leader>wv <C-w>v
nnoremap <Leader>ww <C-w>w
vnoremap <Leader>ww <C-w>w
nnoremap <Leader>wW <C-w>W
vnoremap <Leader>wW <C-w>W
