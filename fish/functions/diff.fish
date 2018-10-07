# diff.fish
# author: Seong Yong-ju <sei40kr@gmail.com>

function diff -w 'diff'
    if command -qs diff-so-fancy
        diff-so-fancy $argv
    else
        diff $argv
    end
end
