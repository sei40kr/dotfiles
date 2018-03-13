typeset -gU fpath infopath manpath path
export TERM='screen-256color-italic'
export ZDOTDIR="${HOME}/.zsh"
export ZSH_CACHE_DIR="${HOME}/.cache/zsh"


## Browser

if [[ "$OSTYPE" == darwin* ]]; then
    export BROWSER='open'
elif [[ "${+commands[xdg-open]}" == 1 ]]; then
    export BROWSER='xdg-open'
fi


## Editor

export EDITOR=emacsclient
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

# Set the Less input preprocessor.
# Try both `lesspipe` and `lesspipe.sh` as either might exist on a system.
if (( $#commands[(i)lesspipe(|.sh)] )); then
    export LESSOPEN="| /usr/bin/env $commands[(i)lesspipe(|.sh)] %s 2>&-"
fi

## Paths

path=( "${HOME}/.local/bin" $path )
fpath=( "${ZDOTDIR}"/{completions,functions} $fpath )


## Homebrew/Linuxbrew

if [[ "$OSTYPE" == darwin* ]]; then
    export BREW_PREFIX='/usr/local'
else
    export BREW_PREFIX="${HOME}/.linuxbrew"
fi

if [[ -x "${BREW_PREFIX}/bin/brew" ]]; then
    infopath=( "${BREW_PREFIX}/share/info" $infopath )
    manpath=( "${BREW_PREFIX}/share/man" $manpath )
    path=( "$BREW_PREFIX"/{bin,sbin} $path )

    # llvm
    if [[ -d "${BREW_PREFIX}/opt/llvm" ]]; then
        path=( "${BREW_PREFIX}/opt/llvm/bin" $path )
        export LD_LIBRARY_PATH="${BREW_PREFIX}/opt/llvm/lib"
    fi
fi


## anyenv

export ANYENV_ROOT="${HOME}/.anyenv"
if [[ -d "$ANYENV_ROOT" ]]; then
    export NDENV_ROOT="${ANYENV_ROOT}/envs/ndenv"
    export PYENV_ROOT="${ANYENV_ROOT}/envs/pyenv"
    export RBENV_ROOT="${ANYENV_ROOT}/envs/rbenv"

    if [[ -d "$NDENV_ROOT" ]]; then
        path=( "${NDENV_ROOT}/shims" $path )
    fi
    if [[ -d "$PYENV_ROOT" ]]; then
        path=( "${PYENV_ROOT}/shims" $path )
    fi
    if [[ -d "$RBENV_ROOT" ]]; then
        path=( "${RBENV_ROOT}/shims" $path )
    fi
fi


## ghq

if [[ "$OSTYPE" == darwin* ]]; then
    export GHQ_ROOT="${HOME}/Develop"
else
    export GHQ_ROOT="${HOME}/dev/ws"
fi


## Golang

export GOPATH="${HOME}/.go"
if [[ ! -d "${GOPATH}/bin" ]]; then
    mkdir -p "${GOPATH}/bin"
fi
path=( "${GOPATH}/bin" $path )


## rustup

export CARGO_HOME="${HOME}/.cargo"
if [[ -d "$CARGO_HOME" ]]; then
    path=( "${CARGO_HOME}/bin" $path )
fi


## SDKMAN!

export SDKMAN_DIR="${HOME}/.sdkman"
if [[ -d "$SDKMAN_DIR" ]]; then
    path=( "$SDKMAN_DIR"/candidates/*/current/bin $path )
fi


## The Haskell Tool Stack

# path=( "${HOME}/.local/bin" $path )


## XDG

if [[ -n "$XDG_SESSION_ID" ]]; then
    export XDG_CONFIG_HOME="${HOME}/.config"

    if [[ -x "${BREW_PREFIX}/bin/brew" ]]; then
        export XDG_DATA_DIRS="${BREW_PREFIX}/share:${XDG_DATA_DIRS}"
    fi
fi
