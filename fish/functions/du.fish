# du.fish
# author: Seong Yong-ju <sei40kr@gmail.com>

function du -w 'ncdu'
    if command -qs ncdu
        ncdu --color dark -rr -x --exclude .git --exclude node_modules $argv
    else
        command du $argv
    end
end
