# __fzf_reverse_isearch.fish
# author: Seong Yong-ju <sei40kr@gmail.com>

function __fzf_reverse_isearch
    if not type -q fzf
        echo 'ERROR: fzf is not installed.' >&2
        return 1
    end

    history | fzf -q (commandline) --toggle-sort=ctrl-r | read result
    and commandline -- $result
    commandline -f repaint
end
