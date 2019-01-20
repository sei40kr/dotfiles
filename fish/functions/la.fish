# la.fish
# author: Seong Yong-ju <sei40kr@gmail.com>

function la
    if command -qs exa
        exa -laFh $argv
    else
        command ls -lAFh $argv
    end
end
