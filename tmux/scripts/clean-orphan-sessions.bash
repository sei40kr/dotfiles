#!/usr/bin/env bash

# clean-orphan-sessions.bash
# author: Seong Yong-ju <sei40kr@gmail.com>

set -euo pipefail
IFS=$'\n\t'

if pgrep tmux >/dev/null; then
    tmux list-sessions -F '#S #{session_attached}' |
        awk '$2==0&&$1~/^[[:digit:]]+$/{print $1}' |
        while read -r session_name; do
            tmux kill-session -t "$session_name"
        done
fi
