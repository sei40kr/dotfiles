export TERM='xterm-256color-italic'

# llvm
if [[ -d "${BREW_PREFIX}/opt/llvm" ]]; then
    export LD_LIBRARY_PATH="${BREW_PREFIX}/opt/llvm/lib"
fi

# ghq
if [[ "$OSTYPE" == darwin* ]]; then
    GHQ_ROOT="${HOME}/Develop"
else
    GHQ_ROOT="${HOME}/dev/ws"
fi
export GHQ_ROOT

# xdg-*
export XDG_CONFIG_HOME="${HOME}/.config"
if [[ "$BREW_PREFIX" != "" ]]; then
    export XDG_DATA_DIRS="${BREW_PREFIX}/share:${XDG_DATA_DIRS}"
fi

typeset -U fpath
fpath=(
    "${ZDOTDIR}/completions"
    "${ZDOTDIR}/functions"
    "${fpath[@]}"
)
