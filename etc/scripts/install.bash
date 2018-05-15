#!/usr/bin/env bash

# install.bash
# author: Seong Yong-ju <sei40kr@gmail.com>

set -euo pipefail
IFS=$'\n\t'

DOTFILES_PATH="$(realpath "$(dirname "$0")/../..")"

cd "$DOTFILES_PATH"

if [[ "$OSTYPE" == darwin* ]]; then
  # Homebrew
  /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
elif [[ "$OSTYPE" == linux* ]]; then
  # Linuxbrew
  sh -c "$(curl -fsSL https://raw.githubusercontent.com/Linuxbrew/install/master/install.sh)"
fi

# clang-format
ln -sF "${DOTFILES_PATH}/clang/clang-format" "${HOME}/.clang-format"

# EditorConfig
ln -sF "${DOTFILES_PATH}/editorconfig/editorconfig" "${HOME}/.editorconfig"

# goenv
GOENV_ROOT="${HOME}/.goenv"
ln -sF "${DOTFILES_PATH}/goenv/goenv" "${GOENV_ROOT}"
eval "$("${GOENV_ROOT}/bin/goenv" init - --no-rehash bash)" && goenv install -s 1.10.1 && goenv global 1.10.1

# Haskell Tool Stack
mkdir -p "${HOME}/.stack"
ln -sF "${DOTFILES_PATH}/stack/config.yml" "${HOME}/.stack/config.yml"

# matplotlib
ln -sF "${DOTFILES_PATH}/matplotlib/matplotlibrc" "${HOME}/.config/matplotlib"

# nvm
NVM_DIR="${HOME}/.nvm"
ln -sF "${DOTFILES_PATH}/nvm/nvm" "${NVM_DIR}"
ln -sF "${DOTFILES_PATH}/nvm/default-packages" "${NVM_DIR}/default-packages"
# shellcheck disable=SC1090
. "${NVM_DIR}/nvm.sh" && nvm install stable && nvm alias default stable

# Prettier
ln -sF "${DOTFILES_PATH}/prettier/prettierrc.js" "${HOME}/.prettierrc.js"

# pyenv
PYENV_ROOT="${HOME}/.pyenv"
ln -sF "${DOTFILES_PATH}/pyenv/pyenv" "${PYENV_ROOT}"
mkdir -p "${PYENV_ROOT}/plugins"
ln -sF "${DOTFILES_PATH}/pyenv/pyenv-ccache" "${PYENV_ROOT}/plugins/pyenv-ccache"
ln -sF "${DOTFILES_PATH}/pyenv/pyenv-default-packages" "${PYENV_ROOT}/plugins/pyenv-default-packages"
ln -sF "${DOTFILES_PATH}/pyenv/default-packages" "${PYENV_ROOT}/default-packages"
ln -sF "${DOTFILES_PATH}/pyenv/pyenv-virtualenv" "${PYENV_ROOT}/plugins/pyenv-virtualenv"
eval "$("${PYENV_ROOT}/bin/pyenv" init - --no-rehash bash)" && pyenv install -s 3.6.5 && pyenv global 3.6.5

# ranger
mkdir -p "${HOME}/.config/ranger"
ln -sF "${DOTFILES_PATH}/ranger/commands.py" "${HOME}/.config/ranger/commands.py"
ln -sF "${DOTFILES_PATH}/ranger/rifle.conf" "${HOME}/.config/ranger/rifle.conf"

# rbenv
RBENV_ROOT="${HOME}/.rbenv"
ln -sF "${DOTFILES_PATH}/rbenv/rbenv" "${RBENV_ROOT}"
mkdir -p "${RBENV_ROOT}/plugins"
ln -sF "${DOTFILES_PATH}/rbenv/rbenv-build" "${RBENV_ROOT}/plugins/rbenv-build"
ln -sF "${DOTFILES_PATH}/rbenv/rbenv-bundler" "${RBENV_ROOT}/plugins/rbenv-bundler"
ln -sF "${DOTFILES_PATH}/rbenv/rbenv-default-gems" "${RBENV_ROOT}/plugins/rbenv-default-gems"
ln -sF "${DOTFILES_PATH}/rbenv/default-gems" "${RBENV_ROOT}/default-gems"
eval "$("${RBENV_ROOT}/bin/rbenv" init - --no-rehash bash)" && rbenv install -s 2.5.1 && rbenv global 2.5.1

# ripgrep
ln -sF "${DOTFILES_PATH}/ripgrep/ripgreprc" "${HOME}/.ripgreprc"

# Spacemacs
ln -sF "${DOTFILES_PATH}/spacemacs/emacs.d" "${HOME}/.emacs.d"
ln -sF "${DOTFILES_PATH}/spacemacs/spacemacs.d" "${HOME}/.spacemacs.d"

# TMUX
ln -sF "${DOTFILES_PATH}/tmux" "${HOME}/.tmux"
ln -sF "${DOTFILES_PATH}/tmux/tmux.conf" "${HOME}/.tmux.conf"

# ZSH
mkdir -p "${HOME}/.config/zsh"
ln -sF "${DOTFILES_PATH}/zsh" "${HOME}/.zsh"
ln -sF "${DOTFILES_PATH}/zshenv" "${HOME}/.zshenv"
