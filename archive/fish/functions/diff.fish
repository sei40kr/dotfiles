# diff.fish
# author: Seong Yong-ju <sei40kr@gmail.com>

function diff -w 'diff-so-fancy'
    if command -qs diff-so-fancy
        diff-so-fancy $argv
    else
        command diff $argv
    end
end
