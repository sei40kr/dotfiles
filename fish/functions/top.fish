# top.fish
# author: Seong Yong-ju <sei40kr@gmail.com>

function top -w 'sudo top'
    if command -qs htop
        sudo htop $argv
    else
        sudo top $argv
    end
end
