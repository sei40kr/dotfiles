#!/usr/bin/env zsh

# .zshenv
# author: Seong Yong-ju ( @sei40kr )

export EDITOR
export HISTFILE
export HISTSIZE
export KEYTIMEOUT
export SAVEHIST
export TERM
export XDG_CONFIG_HOME

EDITOR="$(which nvim)"
HISTFILE="${HOME}/.histfile"
HISTSIZE=1000
KEYTIMEOUT=1
SAVEHIST=1000
XDG_CONFIG_HOME="${HOME}/.config"
ZSH_RC_DIR="${HOME}/.zsh/rc"

[[ -d "${HOME}/.linuxbrew" ]] && {
  XDG_DATA_DIRS="${HOME}/.linuxbrew/share:$XDG_DATA_DIRS";
}
