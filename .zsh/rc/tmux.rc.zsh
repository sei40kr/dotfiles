#!/usr/bin/env zsh

# tmux.rc.zsh
# author: Seong Yong-ju ( @sei40kr )

# Launch tmux
# -----------

if [[ "${+commands[tmux]}" == 1 ]] && [[ "$TERM" == xterm* ]]
then
  { tmux has-session -t global 2>/dev/null || tmux new-session -ds global } \
      && tmux attach-session -t global \
      && exit
fi

# vi: et sw=2 cc=80
