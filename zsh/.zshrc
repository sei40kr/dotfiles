#!/usr/bin/env zsh

# .zshrc
# author: Seong Yong-ju <sei40kr@gmail.com>

# Install zplugin if not exists.
if [[ ! -d "${ZDOTDIR}/.zplugin" ]]; then
  echo 'Info: Installing zplugin.'
  sh -c "$(curl -fsSL https://raw.githubusercontent.com/zdharma/zplugin/master/doc/install.sh)"
fi

. "${ZDOTDIR}/.zplugin/bin/zplugin.zsh"
autoload -Uz _zplugin

if [[ "$XDG_SESSION_DESKTOP" !=  "xmonad" && "$VSCODE_PID" == "" ]]; then
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


## anyenv

if [[ -d "$ANYENV_ROOT" ]]; then
    path=( "${ANYENV_ROOT}/bin" $path )
    . "${ANYENV_ROOT}/bin"
    eval "$(anyenv init - zsh)"
fi


## rustup



## SDKMAN!

if [[ -d "$SDKMAN_DIR" ]]; then
    . "${SDKMAN_DIR}/bin/sdkman-init.sh"
fi
