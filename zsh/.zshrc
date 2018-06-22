# .zshrc
# author: Seong Yong-ju <sei40kr@gmail.com>

if [[ -z "$TMUX" ]] && \
       [[ -z "$EMACS" ]] && \
       [[ -z "$INSIDE_EMACS" ]] && \
       [[ -z "$VSCODE_PID" ]] && \
       command -v tmux >/dev/null; then
    tmux_session='default'

    tmux start-server

    if ! tmux has-session 2>/dev/null; then
        tmux new-session -d -s "$tmux_session"
        tmux set-option -t "$tmux_session" destory-unattached off &>/dev/null
    fi

    exec tmux attach-session -d
elif command -v fish >/dev/null; then
    exec fish
fi
