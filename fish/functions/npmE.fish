# npmE.fish
# author: Seong Yong-ju <sei40kr@gmail.com>

function npmE
    begin
        set -lx PATH (npm bin) $PATH
        eval $argv
    end
end

