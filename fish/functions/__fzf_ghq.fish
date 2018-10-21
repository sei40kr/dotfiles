# __fzf_ghq.fish
# author: Seong Yong-ju <sei40kr@gmail.com>

function __fzf_ghq
    begin
        echo '~/.dotfiles'
        echo '~/.spacemacs.d'
        ghq list --full-path | while read path
            string replace $HOME '~' $path
        end
    end | eval (__fzfcmd) $FZF_DEFAULT_OPTS | read path

    if [ -n $path ]
        commandline -- cd\ (string escape (string replace '~' $HOME $path))
        commandline -f execute
    end

    commandline -f repaint
end
