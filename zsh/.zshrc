# .zshrc
# author: Seong Yong-ju <sei40kr@gmail.com>

if [[ -z "$TMUX" ]] && \
       [[ -z "$EMACS" ]] && \
       [[ -z "$INSIDE_EMACS" ]] && \
       [[ -z "$VSCODE_PID" ]] && \
       command -v tmux >/dev/null; then
    tmux_session='default'

    tmux start-server

    # TODO allow to create multiple sessions when using a tiling window manager
    if ! tmux has-session 2>/dev/null; then
        tmux new-session -d -s "$tmux_session"
        tmux set-option -t "$tmux_session" destory-unattached off &>/dev/null
    fi

    exec tmux -2 attach-session -d
elif command -v fish >/dev/null; then
    exec fish
fi
