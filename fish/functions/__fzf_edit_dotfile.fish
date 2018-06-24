# __fzf_edit_dotfile.fish
# author: Seong Yong-ju <sei40kr@gmail.com>

function __fzf_edit_dotfile
    if [ -z "$DOTFILES_PATH" ]
        echo 'ERROR: DOTFILES_PATH is empty or not defined.' >&2
        return 1
    end

    if not type -q fzf
        echo 'ERROR: fzf is not installed.' >&2
        return 1
    end

    git -C "$DOTFILES_PATH" ls-files -co --exclude-standard | eval (__fzfcmd) $FZF_DEFAULT_OPTS | read dotfile
    and commandline -- emacs\ $dotfile
    and commandline -f execute
end
