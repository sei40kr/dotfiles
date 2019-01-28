# cd-gitroot.fish
# author: Seong Yong-ju <sei40kr@gmail.com>

function cd-gitroot
    git rev-parse --show-toplevel ^/dev/null | read -l a_path
    or begin
        echo "Not in a git repository" >&2
    end

    commandline -- cd\ (string escape $a_path)
    commandline -f execute
end
