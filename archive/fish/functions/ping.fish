# ping.fish
# author: Seong Yong-ju <sei40kr@gmail.com>

function ping -w 'prettyping'
    if command -qs prettyping
        prettyping --nolegend $argv
    else
        command ping $argv
    end
end
