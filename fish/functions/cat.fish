# cat.fish
# author: Seong Yong-ju <sei40kr@gmail.com>

function -w 'cat'
    if command -qs bat
        bat $argv
    else
        cat $argv
    end
end
