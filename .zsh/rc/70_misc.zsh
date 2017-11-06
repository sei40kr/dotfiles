#!/usr/bin/env zsh

# 70_misc.zsh
# author: Seong Yong-ju ( @sei40kr )

# denysdovhan/spaceship-zsh-theme {{{
SPACESHIP_PACKAGE_SHOW=0
SPACESHIP_BATTERY_SHOW=0
# }}}

# junegunn/fzf {{{
if [[ "${+commands[rg]}" == 1 ]]
then
  FZF_DEFAULT_COMMAND='rg --files --hidden --follow -g ".git/*"'
  FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
fi
if [[ "${+commands[bfs]}" == 1 ]]
then
  FZF_ALT_C_COMMAND='bfs -nocolor -hidden -type d -O4 | sed "s/\.\///"'
fi

if [[ -n "$TMUX" ]] && [[ "${+commands[fzf-tmux]}" == 1 ]]
then
  FZF_TMUX=1
  FZF_TMUX_HEIGHT='25%'
fi
# }}}

# mollifier/anyframe {{{
zstyle ':anyframe:selector:fzf-tmux:' command "fzf-tmux -d ${FZF_TMUX_HEIGHT}"
# }}}
