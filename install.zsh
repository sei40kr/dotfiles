#!/usr/bin/env zsh

# install.zsh
# @author Seong Yong-ju ( @sei40kr )

autoload -Uz colors; colors

. ./lib/log-helper.zsh

dotfiles_dir="${HOME}/dotfiles"
[ -d "$dotfiles_dir" ] || {
  -log-fail 'Could not find dotfiles local repository. Aborting.'
  exit 1
}

. ./alacritty/alacritty-setup.zsh
. ./vscode/vscode-setup.zsh

