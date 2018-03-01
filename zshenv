export EDITOR='emacsclient'
export ZDOTDIR="${HOME}/.zsh"

# Homebrew/Linuxbrew
if [[ "$OSTYPE" == darwin* ]]; then
    BREW_PREFIX='/usr/local'
else
    BREW_PREFIX="${HOME}/.linuxbrew"
fi
typeset -U path
typeset -U manpath
typeset -U infopath
if [[ "$BREW_PREFIX" != "" ]]; then
    path=(
        "${BREW_PREFIX}/bin"
        "${BREW_PREFIX}/sbin"
        "${BREW_PREFIX}/opt/llvm/bin"
        "${path[@]}"
    )
    manpath=(
        "${BREW_PREFIX}/share/man"
        "${manpath[@]}"
    )
    infopath=(
        "${BREW_PREFIX}/share/info"
        "${infopath[@]}"
    )
fi

ASDF_DIR="${HOME}/.asdf"
GOPATH="${HOME}/.go"
SDKMAN_DIR="${HOME}/.sdkman"
PYENV_ROOT="${HOME}/.pyenv"

path=(
    "${HOME}/.local/bin"
    # asdf
    "${ASDF_DIR}/bin"
    "${ASDF_DIR}/shims"
    # Cargo
    "${HOME}/.cargo/bin"
    # Cabal
    "${HOME}/.cabal/bin"
    # Go
    "${GOPATH}/bin"
    # SDKMAN!
    "${SDKMAN_DIR}/candidates/ant/current/bin"
    "${SDKMAN_DIR}/candidates/gradle/current/bin"
    "${SDKMAN_DIR}/candidates/java/current/bin"
    "${SDKMAN_DIR}/candidates/kotlin/current/bin"
    "${SDKMAN_DIR}/candidates/maven/current/bin"
    # pyenv
    "${PYENV_ROOT}/bin"
    "${PYENV_ROOT}/shims"
    "${path[@]}"
)

export PATH
export MANPATH
export INFOPATH
