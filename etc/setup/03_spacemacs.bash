#!/usr/bin/env bash

# 03_spacemacs.bash
# author: Seong Yong-ju <sei40kr@gmail.com>

set -euo pipefail
IFS=$'\n\t'

if [[ ! -d "${HOME}/.emacs.d" ]]; then
  echo 'Info: Installing Spacemacs.'
  git clone -b develop https://github.com/syl20bnr/spacemacs ~/.emacs.d
fi
