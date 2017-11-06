#!/usr/bin/env zsh

# zplug_before.zsh
# author: Seong Yong-ju ( @sei40kr )

# lukechilds/zsh-nvm {{{
NVM_LAZY_LOAD=1
NVM_AUTO_USE=1
NVM_SYMLINK_CURRENT=1
# }}}

# ~/.linuxbrew/opt/fzf/shell {{{
FZF_DEFAULT_COMMAND='rg --files --hidden --follow -g ".git/*"'
FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
FZF_ALT_C_COMMAND='bfs -nocolor -hidden -type d -O4 | sed "s/\.\///"'
# }}}

# denysdovhan/spaceship-zsh-theme {{{
SPACESHIP_PACKAGE_SHOW=0
SPACESHIP_BATTERY_SHOW=0
# }}}
