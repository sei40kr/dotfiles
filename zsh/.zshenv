typeset -gU fpath infopath manpath path
export ZSH_CACHE_DIR="${HOME}/.cache/zsh"

if [[ -z "$TERM" ]]; then
  TERM='xterm-256color-italic'
fi
export TERM

## Browser

if [[ "$OSTYPE" == darwin* ]]; then
  export BROWSER='open'
elif [[ "${+commands[xdg-open]}" == 1 ]]; then
  export BROWSER='xdg-open'
fi


## Editor

export EDITOR='e -nw'
export CVSEDITOR="$EDITOR"
export SVN_EDITOR="$EDITOR"
export GIT_EDITOR="$EDITOR"


## Language

export LANGUAGE='en_US.UTF-8'
export LANG="$LANGUAGE"
export LC_ALL="$LANGUAGE"
export LC_CTYPE="$LANGUAGE"


## Pager

export PAGER=less

# Set the default Less options.
# Mouse-wheel scrolling has been disabled by -X (disable screen clearing).
# Remove -X and -F (exit if the content fits on one screen) to enable it.
export LESS='-F -g -i -M -R -S -w -X -z-4'


## Paths

path=( "${HOME}/.local/bin" $path )
fpath=( "${ZDOTDIR}"/{completions,functions} $fpath )

() {
  for candidate in '/usr/share/man' '/usr/local/share/man' '/usr/local/man'; do
    if [[ -d "$candidate" ]]; then
      manpath=( "$candidate" $manpath )
    fi
  done
}

() {
  for candidate in '/usr/local' '/home/linuxbrew/.linuxbrew' "${HOME}/.linuxbrew"; do
    if [[ -x "${candidate}/bin/brew" ]]; then
      export BREW_PREFIX="$candidate"
      break
    fi
  done
}

if [[ -n "$BREW_PREFIX" ]]; then
  path=( "$BREW_PREFIX"/{bin,sbin} $path )
  manpath=( "${BREW_PREFIX}/share/man" $manpath )
  infopath=( "${BREW_PREFIX}/share/info" $infopath )

  # lesspipe
  if [[ -x "${BREW_PREFIX}/bin/lesspipe.sh" ]]; then
    export LESSOPEN="|${BREW_PREFIX}/bin/lesspipe.sh %s"
    export LESS_ADVANCED_PREPROCESSOR=1
  fi

  # cabal
  if [[ -x "${BREW_PREFIX}/bin/cabal" ]]; then
    path=( "${HOME}/.cabal/bin" $path )
  fi

  # xdg
  if [[ -n "$XDG_SESSION_ID" ]]; then
    export XDG_DATA_DIRS="${BREW_PREFIX}/share:${XDG_DATA_DIRS}"
  fi
fi

# cargo
if [[ -d "${HOME}/.cargo/bin" ]]; then
  path=( "${HOME}/.cargo/bin" $path )
  export RUST_SRC_PATH="$(rustc --print sysroot)/lib/rustlib/src/rust/src"
fi

# Haskell Tool Stack
path=( "${HOME}/bin" "${HOME}/.local/bin" $path )

# nvm
if [[ -s "${HOME}/.nvm/nvm.sh" ]]; then
  export NVM_DIR="${HOME}/.nvm"
  path=( "${NVM_DIR}/current/bin" $path )
fi

# pyenv
if [[ -x "${HOME}/.pyenv/bin/pyenv" ]]; then
  export PYENV_ROOT="${HOME}/.pyenv"
  path=( "${PYENV_ROOT}/bin" "${PYENV_ROOT}/shims" $path )
fi

# rbenv
if [[ -x "${HOME}/.rbenv/bin/rbenv" ]]; then
  export RBENV_ROOT="${HOME}/.rbenv"
  path=( "${RBENV_ROOT}/bin" "${RBENV_ROOT}/shims" $path )
fi

# SDKMAN!
if [[ -s "${HOME}/.sdkman/bin/sdkman-init.sh" ]]; then
  export SDKMAN_DIR="${HOME}/.sdkman"
  path=( "$SDKMAN_DIR"/candidates/*/current/bin $path )
fi

# ghq
if [[ "$OSTYPE" == darwin* ]]; then
  export GHQ_ROOT="${HOME}/Develop"
else
  export GHQ_ROOT="${HOME}/dev/ws"
fi

export PATH
export MANPATH
export INFOPATH
