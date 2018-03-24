#!/usr/bin/env bash

# 22_pyenv.bash - pyenv installer
# author: Seong Yong-ju <sei40kr@gmail.com>

set -euo pipefail
IFS=$'\n\t'


## Check dependencies

if [[ ! -x "${HOME}/.anyenv/bin/anyenv" ]]; then
  echo 'Error: pyenv installer requires anyenv.' 1>&2
  exit 1
fi

# Initialize anyenv.
eval "$("${HOME}/.anyenv/bin/anyenv" init - bash)"


## Install pyenv

echo 'Info: Installing pyenv.'
anyenv install -s pyenv

# Initialize pyenv
eval "$("${ANYENV_ROOT}/envs/pyenv/bin/pyenv" init - bash)"


## Install the stable versions of Python2/3

py2_version='2.7.14'
py3_version='3.6.4'

echo "Info: Installing Python v${py2_version}"
pyenv install -s "$py2_version"

echo "Info: Installing Python v${py3_version}"
pyenv install -s "$py3_version"

# Set the installed versions of Python2/3 as default
pyenv global "${py2_version}" "${py3_version}"


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
  vim-vint
  yamllint
  yapf
)

# Install Python2 packages
echo 'Info: Install Python2 packages'
pip2 install "${pip2_packages[@]}"

# Install Python3 packages
echo 'Info: Install Python3 packages'
pip3 install "${pip3_packages[@]}"

pyenv rehash
