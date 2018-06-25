# upgrade.fish
# author: Seong Yong-ju <sei40kr@gmail.com>

function upgrade
    switch (uname)
        case Linux
            if type -q trizen
                trizen -Syu
            else
                sudo pacman -Syu
            end
        case Darwin
            if not type -q brew
                echo 'ERROR: Homebrew is not installed.' >&2
                return 1
            end

            brew update
            and brew upgrade
    end
end
