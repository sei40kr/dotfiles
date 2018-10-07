# ping.fish
# author: Seong Yong-ju <sei40kr@gmail.com>

function -w 'ping'
    if command -qs prettyping
        prettyping --nolegend $argv
    else
        ping $argv
    end
end
