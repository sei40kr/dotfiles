# ls.fish
# author: Seong Yong-ju <sei40kr@gmail.com>

function ls -w 'exa'
    if command -qs exa
        exa $argv
    else
        command ls $argv
    end
end
