#!/usr/bin/env bash

# update.bash
# author: Seong Yong-ju <sei40kr@gmail.com>

set -euo pipefail
IFS=$'\n\t'

DOTFILES_PATH="$(realpath "$(dirname "$0")/../..")"

cd "$DOTFILES_PATH"

git submodule --init --remote --merge
zsh -lc 'brew update && brew upgrade --all && brew cleanup'
