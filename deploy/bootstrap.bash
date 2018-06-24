#!/usr/bin/env bash

# bootstrap.bash
# author: Seong Yong-ju <sei40kr@gmail.com>

set -euo pipefail
IFS=$'\n\t'

: ${PYENV_ROOT:=${HOME}/.pyenv}

if [[ ! -d "${PYENV_ROOT}" ]]; then
    echo "ERROR: pyenv is not found in PYENV_ROOT: PYENV_ROOT=${PYENV_ROOT}"
    exit 1
fi

if [[ ! -x "${PYENV_ROOT}/shims/python3" ]]; then
    echo "ERROR: python3 is not installed via pyenv."
    exit 1
fi

if [[ ! -x "${PYENV_ROOT}/shims/ansible-playbook" ]]; then
    echo "ERROR: ansible-playbook is not installed via pyenv."
    exit 1
fi

basedir="$(dirname "$(realpath "$0")")"
dotfiles_path="$(realpath "$basedir/..")"

ANSIBLE_NOCOWS=1 "${PYENV_ROOT}/shims/ansible-playbook" \
              -e ansible_connection=local \
              -e ansible_python_interpreter="${PYENV_ROOT}/shims/python3" \
              -e dotfiles_path="$dotfiles_path" \
              -i "${basedir}/inventory" \
              "${basedir}/defaults/main.yml" \
              "$@"
