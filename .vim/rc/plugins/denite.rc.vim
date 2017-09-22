" denite.rc.vim
" author: Seong Yong-ju ( @sei40kr )

call denite#custom#option('_', {
    \   'cursor_wrap': 1,
    \   'highlight_mode_insert': 'WildMenu',
    \   'short_source_names': 1,
    \   'smartcase': 1,
    \   'statusline': !exists('g:loaded_lightline'),
    \ })

call denite#custom#alias('source', 'file_rec/git', 'file_rec')
call denite#custom#var('file_rec/git', 'command', ['git', 'ls-files', '-co', '--exclude-standard'])
call denite#custom#option('file-rec', {
    \   'auto_resize': 1,
    \   'default_action': 'tabswitch',
    \   'direction': 'topleft',
    \   'winheight': 10,
    \ })

call denite#custom#option('grep', {
    \   'default_action': 'tabswitch',
    \   'direction': 'botright',
    \   'winheight': 10,
    \ })

call denite#custom#option('help', {
    \   'direction': 'botright',
    \   'split': 'vertical',
    \   'winwidth': 30,
    \ })

call denite#custom#option('outline', {
    \   'default_action': 'tabswitch',
    \   'direction': 'botright',
    \   'split': 'vertical',
    \   'winwidth': 30,
    \ })

call denite#custom#option('search', {
    \   'direction': 'botright',
    \   'winheight': 10,
    \ })

call denite#custom#map('normal', '<Esc>', '<denite:quit>', 'noremap')
call denite#custom#map('normal', 'sp', '<denite:do_action:split>', 'noremap')
call denite#custom#map('normal', 'vs', '<denite:do_action:vsplit>', 'noremap')

call denite#custom#map('insert', '<C-j>', '<denite:move_to_next_line>', 'noremap')
call denite#custom#map('insert', '<C-k>', '<denite:move_to_previous_line>', 'noremap')

