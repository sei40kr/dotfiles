# ls.fish
# author: Seong Yong-ju <sei40kr@gmail.com>

function ls -w 'exa'
    if command -qs exa
        exa -F $argv
    else
        command ls -F $argv
    end
end
