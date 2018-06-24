#!/usr/bin/env bash

# bootstrap.bash
# author: Seong Yong-ju <sei40kr@gmail.com>

set -euo pipefail
IFS=$'\n\t'

_yes_or_no() {
    read -p "${1} [y/n] " ans
    case "$ans" in
        [Yy]* )
            return 0
            ;;
        [Nn]* )
            return 1
            ;;
        * )
            echo 'Please answer y or n.'
            _yes_or_no "$1"
    esac
}

: ${PYENV_ROOT:=${HOME}/.pyenv}

if [[ ! -d "${PYENV_ROOT}" ]]; then
    echo "INFO: pyenv is not found in PYENV_ROOT: PYENV_ROOT=${PYENV_ROOT}"

    if _yes_or_no 'Would you like install pyenv?'; then
        if ! hash git 2>/dev/null; then
            echo "ERROR: git is not installed." >&2
            exit 1
        fi

        git clone https://github.com/pyenv/pyenv.git "$PYENV_ROOT"
        echo "INFO: pyenv is installed to ${PYENV_ROOT}"
    fi
fi

if [[ ! -x "${PYENV_ROOT}/shims/python3" ]]; then
    echo "ERROR: python3 is not installed via pyenv." >&2
    exit 1
fi

if [[ ! -x "${PYENV_ROOT}/shims/ansible-playbook" ]]; then
    echo "ERROR: ansible-playbook is not installed via pyenv." >&2
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
