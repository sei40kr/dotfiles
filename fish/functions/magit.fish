# magit.fish
# author: Seong Yong-ju <sei40kr@gmail.com>

function magit
    eeval '(progn
             (magit-status-internal "'(string escape -n (realpath .))'")
             (x-focus-frame nil))' >/dev/null
end
