# cat.fish
# author: Seong Yong-ju <sei40kr@gmail.com>

function cat -w 'cat'
    if command -qs bat
        bat --theme TwoDark $argv
    else
        command cat $argv
    end
end
