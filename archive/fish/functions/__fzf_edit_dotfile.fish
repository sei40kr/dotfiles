# __fzf_edit_dotfile.fish
# author: Seong Yong-ju <sei40kr@gmail.com>

function __fzf_edit_dotfile
    git -C ~/.dotfiles ls-files -co --exclude-standard | eval (__fzfcmd) $FZF_DEFAULT_OPTS | read dotfile
    and commandline -- $EDITOR\ $dotfile
    and commandline -f execute
end
