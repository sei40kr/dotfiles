# ssh.fish
# author: Seong Yong-ju <sei40kr@gmail.com>

function ssh -w ssh
    env TERM=xterm-256color ssh $argv
end
