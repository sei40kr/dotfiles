#!/usr/bin/env zsh

# .zshrc
# author: Seong Yong-ju ( @sei40kr )

# Launch tmux if it's not running
[ -z "$TMUX" ] && { tmux new-session; exit; }

# zmodload zsh/zprof
zmodload zsh/zpty

autoload -Uz add-zsh-hook cdr chpwd_recent_dirs zmv

# Configure zsh features
export TERM='screen-256color'
export EDITOR="$(which nvim)"
export HISTFILE="${HOME}/.histfile"
export HISTSIZE=1000
export SAVEHIST=1000
export KEYTIMEOUT=1
export XDG_CONFIG_HOME="${HOME}/.config"
setopt append_history extended_history share_history hist_ignore_all_dups hist_ignore_space hist_reduce_blanks
setopt auto_list auto_menu
setopt auto_cd auto_pushd pushd_ignore_dups
setopt glob_dots
setopt interactive_comments
setopt print_eight_bit
setopt prompt_subst
# Disable beep and its visual effect
setopt no_beep
unsetopt list_beep

# ============ Keymap =============
bindkey -e
bindkey '^[[1;3C' forward-word
bindkey '^[[1;3D' backward-word
bindkey '^[[Z' reverse-menu-complete

# Use working directory history
add-zsh-hook chpwd chpwd_recent_dirs

# Configure version managers for Python, Ruby
eval "$(pyenv init -)"
eval "$(rbenv init -)"

# Configure package managers, Cargo, Golang, Cabal, NPM
export GOPATH="${HOME}/.go"
PATH="${HOME}/.local/bin:${HOME}/.cargo/bin:${GOPATH}/bin:${HOME}/.cabal/bin:${HOME}/.npm-global/bin:${PATH}"
PATH="$(gem env GEM_PATHS):${PATH}"

# Load zsh plugins
source "${HOME}/.zsh/rc/plugins.rc.zsh"

# Integrate zsh plugins with tmux
[ -n "$TMUX" ] && {
  FZF_TMUX=1;
  FZF_TMUX_HEIGHT='25%';
  zstyle ':anyframe:selector:fzf-tmux:' command 'fzf-tmux --extended -d 25%';
}

# Configure a zsh plugin, anyframe
# https://github.com/mollifier/anyframe
bindkey '^r' anyframe-widget-execute-history
bindkey '^xb' anyframe-widget-cdr
bindkey '^xk' anyframe-widget-kill
bindkey '^x^k' anyframe-widget-kill
bindkey '^xe' anyframe-widget-insert-git-branch
bindkey '^x^e' anyframe-widget-insert-git-branch
bindkey '^x^b' anyframe-widget-checkout-git-branch
bindkey '^xg' anyframe-widget-cd-ghq-repository
bindkey '^x^g' anyframe-widget-cd-ghq-repository

# Configure a zsh theme, powerlevel9k
# https://github.com/bhilburn/powerlevel9k
POWERLEVEL9K_LEFT_PROMPT_ELEMENTS=( dir background_jobs )
POWERLEVEL9K_RIGHT_PROMPT_ELEMENTS=( status vcs )
POWERLEVEL9K_SHORTEN_DIR_LENGTH=3
POWERLEVEL9K_SHORTEN_DELIMITER='…'
POWERLEVEL9K_DIR_PATH_SEPARATOR=" $(print_icon 'LEFT_SUBSEGMENT_SEPARATOR') "
POWERLEVEL9K_STATUS_NICE_EXIT_CODE=true
ZLE_RPROMPT_INDENT=0

# Configure a zsh plugin, alias-tips
# https://github.com/djui/alias-tips
export ZSH_PLUGINS_ALIAS_TIPS_TEXT='Use alias: '

# Load user-defined aliases
# Note: This should be the final line of this file
source "${HOME}/.zsh/rc/aliases.rc.zsh"

