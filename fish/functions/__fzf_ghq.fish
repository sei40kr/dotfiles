# __fzf_ghq.fish
# author: Seong Yong-ju <sei40kr@gmail.com>

function __fzf_ghq_fzf
    if [ -z $TMUX ]
        fzf
    else
        if not set -q FZF_TMUX_HEIGHT
            set FZF_TMUX_HEIGHT 40%
        end

        fzf-tmux -d$FZF_TMUX_HEIGHT
    end
end

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
    end | __fzf_ghq_fzf | read repo_path

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
    set -l current_session (tmux display-message -p '#S')

    if [ $repo_name = $current_session ]
        commandline -f repaint
        return
    end

    tmux switch-client -t $repo_name ^/dev/null
    or if not string match -qr '^\d+$' $current_session
        tmux new-session -dc $repo_path -s $repo_name
        tmux switch-client -t $repo_name
    else
        __fzf_ghq_cd $repo_path
        tmux rename-session -t $current_session -- $repo_name
    end
end
