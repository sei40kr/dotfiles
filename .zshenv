#!/usr/bin/env zsh

# .zshenv
# author: Seong Yong-ju ( @sei40kr )

export TERM='screen-256color-italic'

export EDITOR="${commands[nvim]:-$EDITOR}"
export PAGER="${commands[less]:-$PAGER}"
export HISTFILE="${HOME}/.histfile"
export HISTSIZE=1000
export KEYTIMEOUT=1
export SAVEHIST=1000

