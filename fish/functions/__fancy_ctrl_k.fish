# __fancy_ctrl_k.fish
# author: Seong Yong-ju <sei40kr@gmail.com>

function __fancy_ctrl_k
    if [ -z (commandline) ]
        commandline -- fg
        commandline -f execute
    else
        commandline -f kill-line
    end
end
