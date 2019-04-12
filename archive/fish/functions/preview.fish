# preview.fish
# author: Seong Yong-ju <sei40kr@gmail.com>

function preview
    if command -qs bat
        fzf --preview "bat --color 'always' {}"
    else
        fzf --preview "cat --color 'always' {}"
    end
end
