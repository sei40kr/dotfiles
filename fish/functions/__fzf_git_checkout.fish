# __fzf_git_checkout.fish
# author: Seong Yong-ju <sei40kr@gmail.com>

function __fzf_git_checkout
    if not type -q git
        echo 'ERROR: git is not installed.' >&2
        return 1
    end

    if not git rev-parse ^/dev/null
        echo 'ERROR: Not a git repository.' >&2
        return 1
    end

    if not type -q fzf
        echo 'ERROR: fzf is not installed.' >&2
        return 1
    end

    git branch | grep -Pve '^\*' | sed 's/^ *//' | fzf | read git_branch
    and commandline -- "git checkout $git_branch"
    and commandline -f execute
    commandline -f repaint
end
