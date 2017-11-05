#!/usr/bin/env zsh

# .zshrc
# author: Seong Yong-ju ( @sei40kr )

export TERM='xterm-256color-italic'

if [[ "${+commands[tmux]}" == 1 ]] && [[ -z "$TMUX" ]]
then
  env \
      FZF_TMUX=1 \
      FZF_TMUX_HEIGHT='25%' \
      tmux new-session
  exit
fi

# zmodload zsh/zprof
zmodload zsh/zpty

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

if [[ -e "${HOME}/.zsh_secret" ]]
then
  . "${HOME}/.zsh_secret"
fi

autoload -Uz add-zsh-hook
autoload -Uz cdr
autoload -Uz chpwd_recent_dirs
autoload -Uz zmv

setopt append_history
setopt auto_cd
setopt auto_list
setopt auto_menu
setopt auto_pushd
setopt extended_history
setopt glob_dots
setopt hist_ignore_all_dups
setopt hist_ignore_space
setopt hist_reduce_blanks
setopt interactive_comments
setopt no_beep
setopt print_eight_bit
setopt prompt_subst
setopt pushd_ignore_dups
setopt share_history
unsetopt list_beep

export GOPATH="${HOME}/.go"
PYENV_ROOT="${HOME}/.pyenv"
RBENV_ROOT="${HOME}/.rbenv"
ZPLUG_HOME="${HOME}/.zplug"

path=(
  '/usr/local/opt/coreutils/libexec/gnubin'
  '/usr/local/share/git-core/contrib'
  "${HOME}/.cabal/bin"
  "${HOME}/.cargo/bin"
  "${GOPATH}/bin"
  "${PYENV_ROOT}/bin"
  "${RBENV_ROOT}/bin"
  "${path[@]}"
)

source /dev/stdin <<EOM
$(pyenv init - zsh --no-rehash)
$(rbenv init - zsh --no-rehash)
EOM

. "${ZPLUG_HOME}/init.zsh"

. "${HOME}/.zsh/rc/zplug_before.zsh"
. "${HOME}/.zsh/rc/zplug.zsh"
. "${HOME}/.zsh/rc/zplug_lazy_before.zsh"
. "${HOME}/.zsh/rc/zplug_lazy.zsh"

zplug load

# if (which zprof > /dev/null 2>&1) ;then
#   zprof
# fi
