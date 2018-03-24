#!/usr/bin/env bash

# 402_version-manager-anyenv-pyenv.bash - pyenv installer
# author: Seong Yong-ju <sei40kr@gmail.com>

set -euo pipefail
IFS=$'\n\t'

# shellcheck source=helpers/init-helpers.bash
. "$(dirname "${BASH_SOURCE[0]}")/helpers/init-helpers.bash"


## Check dependencies

TRACE 'Checking anyenv is installed.'
if [[ ! -x "${HOME}/.anyenv/bin/anyenv" ]]; then
  FATAL 'pyenv installer requires anyenv. Install anyenv first.'
  exit 1
fi

# Initialize anyenv.
TRACE 'Initializing anyenv.'
eval "$("${HOME}/.anyenv/bin/anyenv" init - bash)" | TRACE


## Install pyenv

INFO 'Installing pyenv.'
anyenv install -s pyenv | DEBUG

# Initialize pyenv
TRACE 'Initializing pyenv.'
eval "$("${ANYENV_ROOT}/envs/pyenv/bin/pyenv" init - bash)" | TRACE


## Install the stable versions of Python2/3.

py2_version='2.7.14'
py3_version='3.6.4'

INFO "Installing Python v${py2_version}"
pyenv install -s "$py2_version" | DEBUG

INFO "Installing Python v${py3_version}"
pyenv install -s "$py3_version" | DEBUG

# Set the installed versions of Python2/3 as defaults.
INFO "Setting Python v${py2_version} and v${py3_version} as defaults."
pyenv global "${py2_version}" "${py3_version}" | DEBUG


## Install Python packages

pip2_packages=(
  ansible-lint
)
pip3_packages=(
  asciinema
  autopep8
  aws-shell
  cmakelint
  cpplint
  flake8
  jedi
  jupyter
  mycli
  pgcli
  proselint
  ptpython
  pygments
  pylint
  python-language-server
  pyls-mypy
  pyls-isort
  vim-vint
  yamllint
  yapf
)

# Install Python2 packages
INFO 'Install Python2 packages'
pip2 install "${pip2_packages[@]}" | DEBUG

# Install Python3 packages
INFO 'Install Python3 packages'
pip3 install "${pip3_packages[@]}" | DEBUG

INFO 'Rehashing Python packages.'
pyenv rehash | DEBUG
