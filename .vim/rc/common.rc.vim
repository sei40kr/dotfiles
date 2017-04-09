" common.rc.vim
" author: Seong Yong-ju ( @sei40kr )
"
" There are common settings used in Vim/Neovim/IdeaVim in this file.

set hlsearch ignorecase smartcase wrapscan
set matchpairs& matchpairs+=<:>
set iskeyword& iskeyword+=-,/,?
set nonumber relativenumber
set showmode
set ttimeout ttimeoutlen=100

noremap <silent> <C-c> <Esc>
nnoremap ; :
inoremap <silent> jj <Esc>

" Disable command-line history
nnoremap <silent> q: <Nop>
" Disable Ex-mode
nnoremap <silent> Q <Nop>
" Disable tab closing
nnoremap <silent> ZQ <Nop>

" Empty line insertions
nnoremap <silent> [<Space> O<Esc>j
nnoremap <silent> ]<Space> o<Esc>k

" Navigate putting the line vertically centered
nnoremap <silent> n nzz
nnoremap <silent> N Nzz
nnoremap <silent> * *zz
nnoremap <silent> # #zz
nnoremap <silent> g* g*zz
nnoremap <silent> g# g#zz

" Input Esc twice to clear the highlights of
nnoremap <silent> <Esc><Esc> :<C-u>nohlsearch<CR>

" Save or close the current buffer
nnoremap <silent> <Space>q :<C-u>q<CR>
nnoremap <silent> <Space>w :<C-u>w<CR>

" Move between editor panes
nnoremap <silent> <C-h> <C-w>h
nnoremap <silent> <C-j> <C-w>j
nnoremap <silent> <C-k> <C-w>k
nnoremap <silent> <C-l> <C-w>l

