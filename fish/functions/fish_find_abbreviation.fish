# fish_find_abbreviation.fish
# author: Seong Yong-ju <sei40kr@gmail.com>

function fish_find_abbreviation
    if not type -q fzf
        echo 'ERROR: fzf is not installed.' >&2
        return 1
    end

    abbr -s | sed 's/^abbr //' | fzf | awk '{ print $1 }' | read abbr
    commandline -- $abbr
end
