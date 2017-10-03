" common.rc.vim
" author: Seong Yong-ju ( @sei40kr )

set hlsearch
set ignorecase
set iskeyword& iskeyword+=-,/,?
set matchpairs& matchpairs+=<:>
set number
set relativenumber
set showmode
set smartcase
set ttimeout ttimeoutlen=100
set wrapscan

noremap <C-c> <Esc>

nnoremap Q <Nop>
nnoremap <silent> q: <Nop>

nnoremap <silent> [<Space> O<Esc>j
nnoremap <silent> ]<Space> o<Esc>k

nnoremap <silent> <Esc><Esc> :<C-u>nohlsearch<CR>

nnoremap <silent> <Space>q :<C-u>q<CR>
nnoremap <silent> <Space>w :<C-u>w<CR>

vnoremap <silent> < <gv
vnoremap <silent> > >gv

