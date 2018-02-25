#!/usr/bin/env zsh

# exports.rc.zsh
# author: Seong Yong-ju ( @sei40kr )

typeset -U path
typeset -U fpath
typeset -U manpath
typeset -U infopath

# Homebrew/Linuxbrew
# ------------------

if [[ -d "/home/linuxbrew/.linuxbrew" ]]
then
  BREW_PREFIX="/home/linuxbrew/.linuxbrew"
elif [[ -d "${HOME}/.linuxbrew" ]]
then
  BREW_PREFIX="${HOME}/.linuxbrew"
elif [[ -x '/usr/local/bin/brew' ]]
then
  BREW_PREFIX='/usr/local'
fi

if [[ -n "$BREW_PREFIX" ]]
then
  path=(
    "${BREW_PREFIX}/bin"
    "${BREW_PREFIX}/sbin"
    "${path[@]}"
  )
  manpath=(
    "${BREW_PREFIX}/share/man:${MANPATH}"
    "${manpath[@]}"
  )
  infopath=(
    "${BREW_PREFIX}/share/info:${INFOPATH}"
    "${infopath[@]}"
  )

  XDG_DATA_DIRS="${BREW_PREFIX}/share:${XDG_DATA_DIRS}"
fi

# If LLVM was installed via Homebrew
if [[ -d "${BREW_PREFIX}/opt/llvm" ]]
then
  path=(
    "${BREW_PREFIX}/opt/llvm/bin"
    "${path[@]}"
  )
  LD_LIBRARY_PATH="${BREW_PREFIX}/opt/llvm/lib"
fi


# Golang
# ------

if [[ -d "${HOME}/.go" ]]
then
  GOPATH="${HOME}/.go"
  path=( "${GOPATH}/bin" "${path[@]}" )
fi

path=(
  "${HOME}/.local/bin"(N-/)
  "${HOME}/.cabal/bin"(N-/)
  "${HOME}/.cargo/bin"(N-/)
  "${path[@]}"
)


# Misc
# ----

if [[ -d "${HOME}/.zsh" ]]
then
  fpath=(
    "${HOME}/.zsh/completions"
    "${HOME}/.zsh/functions"
    "${fpath[@]}"
  )
fi

XDG_CONFIG_HOME="${HOME}/.config"

export PATH
export FPATH
export MANPATH
export INFOPATH
export GOPATH
export BREW_PREFIX
export XDG_CONFIG_HOME
export XDG_DATA_DIRS

# vi: et sw=2 cc=80
