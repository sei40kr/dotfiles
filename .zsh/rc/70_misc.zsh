#!/usr/bin/env zsh

# 70_misc.zsh
# author: Seong Yong-ju ( @sei40kr )

# creationix/nvm {{{
export NVM_SYMLINK_CURRENT=1
# }}}

# denysdovhan/spaceship-zsh-theme {{{
SPACESHIP_PACKAGE_SHOW=0
SPACESHIP_BATTERY_SHOW=0
# }}}

# junegunn/fzf {{{
FZF_DEFAULT_COMMAND='rg --files --hidden --follow -g ".git/*"'
FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
FZF_ALT_C_COMMAND='bfs -nocolor -hidden -type d -O4 | sed "s/\.\///"'

if [[ -n "$TMUX" ]]
then
  FZF_TMUX=1
  FZF_TMUX_HEIGHT='25%'
fi
# }}}

# lukechilds/zsh-nvm {{{
export NVM_LAZY_LOAD=1
export NVM_AUTO_USE=1
# }}}

# mollifier/anyframe {{{
zstyle ':anyframe:selector:fzf-tmux:' command "fzf-tmux -d ${FZF_TMUX_HEIGHT}"
# }}}
