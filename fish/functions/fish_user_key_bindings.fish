# fish_user_key_bindings.fish
# author: Seong Yong-ju <sei40kr@gmail.com>

function fish_user_key_bindings
    bind \cr __fzf_reverse_isearch
    bind \cx\cg __fzf_ghq
end
