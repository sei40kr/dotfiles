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
    bind \ec 'commandline ranger-cd; commandline -f execute'

    # sudope
    bind \cs sudope
end