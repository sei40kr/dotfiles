# tmux.fish
# author: Seong Yong-ju <sei40kr@gmail.com>

function __backward_delete_char_or_select_left_pane
    if [ -n "$TMUX" -a -z (commandline) ]
        tmux select-pane -L
    else
        commandline -f backward-delete-char
    end
end

function __execute_or_select_below_pane
    if [ -n "$TMUX" -a -z (commandline) ]
        tmux select-pane -D
    else
        commandline -f execute
    end
end

function __kill_line_or_select_above_pane
    if [ -n "$TMUX" -a -z (commandline) ]
        tmux select-pane -U
    else
        commandline -f kill-line
    end
end

function __clear_or_select_right_pane
    if [ -n "$TMUX" ]
        tmux select-pane -R
    end
end
