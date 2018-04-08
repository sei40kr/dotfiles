#!/usr/bin/env zsh

# .zshrc
# author: Seong Yong-ju <sei40kr@gmail.com>

. "${ZDOTDIR}/.zplugin/bin/zplugin.zsh"
autoload -Uz _zplugin

if [[ "${+_comps}" == 1 ]]; then
    _comps[zplugin]=_zplugin
fi

. "${ZDOTDIR}/30_aliases.zsh"
. "${ZDOTDIR}/50_options.zsh"
. "${ZDOTDIR}/80_custom.zsh"
. "${ZDOTDIR}/zplugin.zsh"

compinit
zplugin cdreplay -q

# SDKMAN!
if [[ -n "$SDKMAN_DIR" ]]; then
  . "${SDKMAN_DIR}/bin/sdkman-init.sh"
fi
