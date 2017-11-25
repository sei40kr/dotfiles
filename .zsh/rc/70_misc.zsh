#!/usr/bin/env zsh

# 70_misc.zsh
# author: Seong Yong-ju ( @sei40kr )

# junegunn/fzf {{{
FZF_DEFAULT_COMMAND='rg --files --hidden --follow --iglob "!.git/**"'
FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
FZF_ALT_C_COMMAND='bfs -nocolor -hidden -type d -O4 | sed "s/\.\///"'

if [[ -n "$TMUX" ]]
then
  FZF_TMUX=1
  FZF_TMUX_HEIGHT='25%'
fi
# }}}

# mollifier/anyframe {{{
zstyle ':anyframe:selector:fzf-tmux:' command "fzf-tmux -d ${FZF_TMUX_HEIGHT}"
# }}}

