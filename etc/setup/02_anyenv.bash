#!/usr/bin/env bash

# 02_anyenv.bash
# author: Seong Yong-ju <sei40kr@gmail.com>

set -euo pipefail
IFS=$'\n\t'

if [[ ! -d "${HOME}/.anyenv" ]]; then
  echo 'Info: Installing anyenv.'
  git clone --depth=1 -- https://github.com/riywo/anyenv "${HOME}/.anyenv"
fi
