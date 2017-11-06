#!/usr/bin/env zsh

# .zshenv
# author: Seong Yong-ju ( @sei40kr )

export TERM='xterm-256color-italic'

export EDITOR="$(which nvim)"
export HISTFILE="${HOME}/.histfile"
export HISTSIZE=1000
export KEYTIMEOUT=1
export SAVEHIST=1000

export XDG_CONFIG_HOME="${HOME}/.config"

if [[ -d "${HOME}/.linuxbrew" ]]
then
  export XDG_DATA_DIRS="${HOME}/.linuxbrew/share:$XDG_DATA_DIRS";
fi

export GOPATH="${HOME}/.go"
export PYENV_ROOT="${HOME}/.pyenv"
export RBENV_ROOT="${HOME}/.rbenv"
