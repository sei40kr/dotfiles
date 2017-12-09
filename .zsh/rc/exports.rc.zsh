#!/usr/bin/env zsh

# exports.rc.zsh
# author: Seong Yong-ju ( @sei40kr )

# Homebrew/Linuxbrew
# ------------------

if [[ -d "${HOME}/.linuxbrew" ]]
then
  HOMEBREW_PREFIX="${HOME}/.linuxbrew"
elif [[ -x '/usr/local/bin/brew' ]]
then
  HOMEBREW_PREFIX='/usr/local'
fi

if [[ -n "$HOMEBREW_PREFIX" ]]
then
  export HOMEBREW_PREFIX
  path=( "${HOMEBREW_PREFIX}/bin" "${path[@]}" )

  export XDG_DATA_DIRS="${HOMEBREW_PREFIX}/share:${XDG_DATA_DIRS}"
fi


# anyenv
# ------

if [[ -d "${HOME}/.anyenv" ]]
then
  export ANYENV_ROOT="${HOME}/.anyenv"
  path=( "${ANYENV_ROOT}/bin" "${path[@]}" )
fi


# Golang
# ------

if [[ -d "${HOME}/.go" ]]
then
  export GOPATH="${HOME}/.go"
  path=( "${GOPATH}/bin" "${path[@]}" )
fi

path=(
  "${HOME}/.local/bin"(N-/)
  "${HOME}/.cabal/bin"(N-/)
  "${HOME}/.cargo/bin"(N-/)
  "${path[@]}"
)
export PATH


# Misc
# ----

if [[ -d "${HOME}/.zsh" ]]
then
  fpath=(
    "${HOME}/.zsh/completions"
    "${HOME}/.zsh/functions"
    "${fpath[@]}"
  )
  export FPATH
fi

export XDG_CONFIG_HOME="${HOME}/.config"

# vi: et sw=2 cc=80
