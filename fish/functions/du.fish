# du.fish
# author: Seong Yong-ju <sei40kr@gmail.com>

function du -w 'du'
    if command -qs ncdu
        ncdu --color dark -rr -x --exclude .git --exclude node_modules $argv
    else
        du $argv
    end
end
