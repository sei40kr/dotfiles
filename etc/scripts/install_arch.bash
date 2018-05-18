#!/usr/bin/env bash

# install_arch.bash --- dotfiles installer for Arch Linux
# author: Seong Yong-ju <sei40kr@gmail.com>

set -euo pipefail
IFS=$'\n\t'

DOTFILES_PATH="$(realpath "$(dirname "$0")/../..")"

# dropbox
mkdir -p "${HOME}/Dropbox/notes"
systemctl --user enable dropbox

# i3
mkdir -p "${HOME}/.config/i3"
ln -sF "${DOTFILES_PATH}/i3/config" "${HOME}/.config/i3/config"

# i3status
mkdir -p "${HOME}/.config/i3status"
ln -sF "${DOTFILES_PATH}/i3status/config" "${HOME}/.config/i3status/config"

# scrot
mkdir -p "${HOME}/screenshots"
