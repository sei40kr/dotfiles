# __fzf_ghq.fish
# author: Seong Yong-ju <sei40kr@gmail.com>

function __fzf_ghq_cd --argument-names path
    commandline -- cd\ (string escape $path)
    commandline -f execute
end

function __fzf_ghq
    begin
        echo '~/.dotfiles'
        echo '~/.spacemacs.d'
        ghq list --full-path | while read path
            string replace $HOME '~' $path
        end
    end | eval (__fzfcmd) $FZF_DEFAULT_OPTS | read repo_path

    if [ -z $repo_path ]
        commandline -f repaint
        return
    end

    if [ -z $TMUX ]
        __fzf_ghq_cd $repo_path
    end

    set -l repo_path (string replace '~' $HOME $repo_path)
    set -l repo_name (string replace -a '.' '-' \
        (string replace -r '^\.' '' (basename $repo_path)))

    tmux switch-client -t $repo_name ^/dev/null
    or begin
        set -l current_session (tmux display-message -p '#S')

        if not string match -qr '^\d+$' $current_session
            tmux new-session -dc $repo_path -s $repo_name
            tmux switch-client -t $repo_name
        else
            __fzf_ghq_cd $repo_path
            tmux rename-session -t $current_session -- $repo_name
        end
    end
end
