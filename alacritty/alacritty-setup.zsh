#!/usr/bin/env zsh

# alacritty-setup.zsh
# @author Seong Yong-ju ( @sei40kr )

alacritty_home_dir="${HOME}/.config/alacritty"
[ -d "$alacritty_home_dir" ] || mkdir -p "$alacritty_home_dir"

-log-wait 'Symlinking Alacritty settings ...'

uname="$(uname -s)"
if [ "$uname" = 'Darwin' ]
then
  ln -sfv "${dotfiles_dir}/alacritty/alacritty-macos.yml" \
      "${alacritty_home_dir}/alacritty/alacritty.yml"
else
  ln -sfv "${dotfiles_dir}/alacritty/alacritty.yml" \
      "${alacritty_home_dir}/alacritty/alacritty.yml"
fi

-log-done "Symlinks were successfully created."
