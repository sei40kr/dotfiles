#!/usr/bin/env zsh

# aliases.rc.zsh
# author: Seong Yong-ju ( @sei40kr )

alias help='man'

alias u='\cd ..'
alias t='tail -f'
alias grep='grep --color'
alias sortnr='sort -nr'
alias p='ps -f'
alias ls='ls --color=auto'
alias la='ls -a --color=auto'
alias ll='ls -la --color=auto'
alias mkdirp='mkdir -p'
alias less='less -R'
alias dud='du -d 1 -h'
alias zmv='noglob zmv'

alias vim='nvim'

# Docker aliases
alias d='docker'
alias drm='docker rm -f'

alias zshrc="${EDITOR} ~/.zshrc"

# Require a zsh plugin, cd-gitroot
alias U='cd-gitroot'

