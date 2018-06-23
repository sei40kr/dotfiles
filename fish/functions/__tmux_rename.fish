# __tmux_rename.fish
# author: Seong Yong-ju <sei40kr@gmail.com>

function __tmux_rename -v PWD
    if [ -z "$TMUX" ]
        return
    end

    set -lx tmux_window_id (tmux display-message -p '#{window_id}')
    set -lx cwd $PWD

    sh -c 'tmux rename-window -t "$tmux_window_id" "$(basename "$(git -C "$cwd" rev-parse --show-toplevel 2>/dev/null || echo "$cwd")")" &'
end
