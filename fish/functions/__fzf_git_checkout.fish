# __fzf_git_checkout.fish
# author: Seong Yong-ju <sei40kr@gmail.com>

function __fzf_git_checkout
    if not command -qs git
        echo 'ERROR: git not found.' >&2
        return 1
    end

    if not git rev-parse ^/dev/null
        echo 'ERROR: Not a git repository.' >&2
        return 1
    end

    if not command -qs fzf
        echo 'ERROR: fzf not found.' >&2
        return 1
    end

    git branch | awk '$1 != "*" { print $1 }' | eval (__fzfcmd) $FZF_DEFAULT_OPTS | read git_branch
    and commandline -- "git checkout $git_branch"
    and commandline -f execute
    commandline -f repaint
end
