#!/usr/bin/env bash

# bootstrap.bash
# author: Seong Yong-ju <sei40kr@gmail.com>

set -euo pipefail
IFS=$'\n\t'

basepath="$(dirname "$(realpath "$0")")"

echo 'Setting up the local environment ...'
ANSIBLE_NOCOWS=1 ansible-playbook -i "${basepath}/inventory" "${basepath}/defaults/main.yml"
