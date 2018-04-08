#!/usr/bin/env zsh

# .zshrc
# author: Seong Yong-ju <sei40kr@gmail.com>

. "${ZDOTDIR}/.zplugin/bin/zplugin.zsh"
autoload -Uz _zplugin

-should-run-tmux() {
  if [[ -n "$TMUX" ]] \
       || [[ -n "$EMACS" ]] \
       || [[ -n "$VIM" ]] \
       || [[ -n "$INSIDE_EMACS" ]] \
       || [[ -n "$VSCODE_PID" ]] \
       || [[ "$XDG_SESSION_DESKTOP" =~ ^(xfce|xmonad)$ ]]; then
    return 1
  fi
}

if -should-run-tmux; then
    zstyle ':prezto:module:tmux:auto-start' local 'yes'
    zstyle ':prezto:module:tmux:session' name 'default'
    zplugin ice svn; zplugin snippet PZT::modules/tmux
fi

if [[ "${+_comps}" == 1 ]]; then
    _comps[zplugin]=_zplugin
fi


. "${ZDOTDIR}/20_keymap.zsh"
. "${ZDOTDIR}/30_aliases.zsh"
. "${ZDOTDIR}/50_options.zsh"
. "${ZDOTDIR}/80_custom.zsh"
. "${ZDOTDIR}/zplugin.zsh"

compinit
zplugin cdreplay -q

# goenv
if [[ -n "$GOENV_ROOT" ]]; then
  path=( "${GOENV_ROOT}/bin" $path )
  eval "$(goenv init - zsh --no-rehash)"
  zplugin ice wait''; zplugin snippet "${GOENV_ROOT}/completions/goenv.zsh"
fi

# nvm
if [[ -n "$NVM_DIR" ]]; then
  export NVM_SYMLINK_CURRENT=true
  export NVM_LAZY_LOAD=true
  zplugin light lukechilds/zsh-nvm
fi

# pyenv
if [[ -n "$PYENV_ROOT" ]]; then
  path=( "${PYENV_ROOT}/bin" $path )
  eval "$(pyenv init - zsh --no-rehash)"
  zplugin ice wait''; zplugin snippet "${PYENV_ROOT}/completions/pyenv.zsh"
fi

# rbenv
if [[ -n "$RBENV_ROOT" ]]; then
  path=( "${RBENV_ROOT}/bin" $path )
  eval "$(rbenv init - zsh --no-rehash)"
  zplugin ice wait''; zplugin snippet "${RBENV_ROOT}/completions/rbenv.zsh"
fi

# SDKMAN!
if [[ -n "$SDKMAN_DIR" ]]; then
  . "${SDKMAN_DIR}/bin/sdkman-init.sh"
fi
