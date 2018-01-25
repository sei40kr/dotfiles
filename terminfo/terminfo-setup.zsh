#!/usr/bin/env zsh

# terminfo-setup.zsh
# author: Seong Yong-ju ( @sei40kr )

[[ "${+commands[tic]}" == 1 ]] || {
  -log-fail 'Could not find tic executable in PATH. Aborting.'
  exit 1
}

-log-wait 'Installing terminfo ...'

tic "${dotfiles_dir}/terminfo/screen-256color-italic.terminfo"
tic "${dotfiles_dir}/terminfo/xterm-256color-italic.terminfo"

-log-done "terminfo were successfully installed."
