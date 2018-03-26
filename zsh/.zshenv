typeset -gU fpath infopath manpath path
export TERM='screen-256color-italic'
export ZSH_CACHE_DIR="${HOME}/.cache/zsh"


## Browser

if [[ "$OSTYPE" == darwin* ]]; then
  export BROWSER='open'
elif [[ "${+commands[xdg-open]}" == 1 ]]; then
  export BROWSER='xdg-open'
fi


## Editor

export EDITOR="${EMACS_PLUGIN_LAUNCHER} -nw"
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

  # llvm
  if [[ -d "${BREW_PREFIX}/opt/llvm" ]]; then
    export LD_LIBRARY_PATH="${BREW_PREFIX}/opt/llvm/lib"
    export LDFLAGS="-L${LD_LIBRARY_PATH} -Wl,-rpath,${LD_LIBRARY_PATH}"
    export CPPFLAGS="-I${BREW_PREFIX}/opt/llvm/include"
    path=( "${BREW_PREFIX}/opt/llvm/bin" $path )
  fi

  # goenv
  if [[ -x "${BREW_PREFIX}/opt/goenv/bin/goenv" ]]; then
    export GOENV_ROOT="${BREW_PREFIX}/opt/goenv"
    export GOPATH="${HOME}/.go"
    path=( "${GOENV_ROOT}/shims" "${GOPATH}/bin" $path )
  fi

  # nvm
  if [[ -s "${BREW_PREFIX}/opt/nvm/nvm.sh" ]]; then
    export NVM_DIR="${BREW_PREFIX}/opt/nvm"
    path=( "${NVM_DIR}/current/bin" $path )
  fi

  # pyenv
  if [[ -x "${BREW_PREFIX}/opt/pyenv/bin/pyenv" ]]; then
    export PYENV_ROOT="${BREW_PREFIX}/opt/pyenv"
    path=( "${PYENV_ROOT}/shims" $path )
  fi

  # rbenv
  if [[ -x "${BREW_PREFIX}/opt/rbenv/bin/rbenv" ]]; then
    export RBENV_ROOT="${BREW_PREFIX}/opt/rbenv"
    path=( "${RBENV_ROOT}/shims" $path )
  fi

  # xdg
  if [[ -n "$XDG_SESSION_ID" ]]; then
    export XDG_DATA_DIRS="${BREW_PREFIX}/share:${XDG_DATA_DIRS}"
  fi
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
