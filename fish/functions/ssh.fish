# ssh.fish
# author: Seong Yong-ju <sei40kr@gmail.com>

function ssh -w ssh
    env TERM=xterm ssh $argv
end
