# upgrade.fish
# author: Seong Yong-ju <sei40kr@gmail.com>

function upgrade
    if type -q trizen
        trizen -Syu
    else
        sudo pacman -Syu
    end
end
