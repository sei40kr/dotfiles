# __fzf_ghq.fish
# author: Seong Yong-ju <sei40kr@gmail.com>

function __fzf_ghq_home_to_tilde
    # FIXME fish shell can't read the standard input from function
    # cf https://github.com/fish-shell/fish-shell/issues/206
    while read -l a_path
        string replace -r '^'(string escape -n --style=regex $HOME) '~' $a_path
    end
end

function __fzf_ghq_fzf
    if [ -z $TMUX ]
        fzf --reverse
    else
        set -q FZF_TMUX_HEIGHT
        or set -l FZF_TMUX_HEIGHT 40%

        fzf-tmux -d$FZF_TMUX_HEIGHT --reverse
    end
end

function __fzf_ghq_tilde_to_home
    while read -l a_path
        string replace -r '^~' $HOME $a_path
    end
end

function __fzf_ghq_cd -a path
    commandline -- cd\ (string escape $path)
    commandline -f execute
end

function __fzf_ghq
    begin
        [ -d $HOME'/.dotfiles' ]
        and echo $HOME'/.dotfiles'
        [ -d $HOME'/.emacs.d' ]
        and echo $HOME'/.emacs.d'

        set -q GHQ_ROOT
        or set -l GHQ_ROOT "$HOME/.ghq"
        find $GHQ_ROOT -mindepth 3 -maxdepth 3 -type d
    end | __fzf_ghq_home_to_tilde | __fzf_ghq_fzf | __fzf_ghq_tilde_to_home | read -l a_path

    # 1. Exit if no project were selected
    or return

    # 2. If the current session were not attached to tmux, just run `cd`
    if [ -z $TMUX ]
        __fzf_ghq_cd $a_path
        return
    end

    set -l session_name (string replace -a '.' '' (basename $a_path))

    # 3. If a session for the project existed (or it were the current one),
    #    just switch to it
    tmux switch-client -t $session_name ^/dev/null
    and return

    set -l current_session (tmux display-message -p '#S')

    # 4. If the current session hadn't renamed, make it up for the project
    if string match -qr '^\d+$' $current_session
        tmux rename-session -t $current_session -- $session_name
        __fzf_ghq_cd $a_path
    else
        # 5. Create a session for the project
        tmux new-session -dc $a_path -s $session_name
        tmux switch-client -t $session_name
    end
end
