# -*- mode: sh -*-

# zshenv
# author: Seong Yong-ju <sei40kr@gmail.com>

export ZDOTDIR="${HOME}/.zsh"

# We can't set environment variables in iTerm2 profile. So setting locale
# variables here.
# See https://gitlab.com/gnachman/iterm2/-/issues/5307
if [[ -n "$ITERM_SESSION_ID" && -z "$LANG" ]]; then
    export LANG=ja_JP.UTF-8
fi

export EDITOR=vim
export GIT_EDITOR="$EDITOR"
