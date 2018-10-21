# tree.fish
# author: Seong Yong-ju <sei40kr@gmail.com>

function tree -w 'exa -T'
    if command -qs exa
        exa -T $argv
    else
        command tree $argv
    end
end
