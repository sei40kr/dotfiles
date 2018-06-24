# __fzf_ghq.fish
# author: Seong Yong-ju <sei40kr@gmail.com>

function __fzf_ghq
    if not type -q ghq
        echo 'ERROR: ghq is not installed.' >&2
        return 1
    end

    if not type -q fzf
        echo 'ERROR: fzf is not installed.' >&2
        return 1
    end

    begin
        echo $HOME/.dotfiles
        echo $HOME/.emacs.d
        ghq list --full-path
    end | eval (__fzfcmd) $FZF_DEFAULT_OPTS | read repo_dir
    and commandline -- cd\ {$repo_dir}
    and commandline -f execute
    commandline -f repaint
end
