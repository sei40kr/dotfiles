#!/usr/bin/env bash

# 01_brew.bash
# author: Seong Yong-ju <sei40kr@gmail.com>

set -euo pipefail
IFS=$'\n\t'

case "$OSTYPE" in
  darwin*)
    echo 'Info: Installing Homebrew.'
    # cf https://brew.sh
    /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
    ;;
  linux*)
    echo 'Info: Installing Linuxbrew.'
    # cf http://linuxbrew.sh
    sh -c "$(curl -fsSL https://raw.githubusercontent.com/Linuxbrew/install/master/install.sh)"
    ;;
esac
