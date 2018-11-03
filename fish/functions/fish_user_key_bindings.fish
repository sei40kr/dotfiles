# fish_user_key_bindings.fish
# author: Seong Yong-ju <sei40kr@gmail.com>

function fish_user_key_bindings
    bind \cx. __fzf_edit_dotfile
    bind \cx\cb __fzf_git_checkout

    # fish-ghq
    bind \cx\cg __fzf_ghq

    # fzf
    bind \ct __fzf_find_file
    bind \cr __fzf_reverse_isearch
    bind \ec __fzf_cd
    bind \eC '__fzf_cd --hidden'
    bind \cg __fzf_open
    # bind \co '__fzf_open --editor'

    # ranger
    bind \co 'commandline ranger-cd; commandline -f execute'

    # sudope
    bind \cs sudope

    # tmux
    bind \ch __backward_delete_char_or_select_left_pane
    bind \cj __execute_or_select_below_pane
    bind \ck __kill_line_or_select_above_pane
    bind \cl __clear_or_select_right_pane
end
