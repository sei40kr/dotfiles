#!/usr/bin/env zsh

# vscode-setup.zsh
# @author Seong Yong-ju ( @sei40kr )

[[ "${+commands[code]}" == 1 ]] || {
  -log-fail 'Could not found code executable in PATH. Aborting.'
  exit 1
}

uname="$(uname -s)"
if [ "$uname" = 'Darwin' ]
then
  vscode_home_dir="${HOME}/Library/Application Support/Code"
else
  vscode_home_dir="${HOME}/.config/Code"
  [ -d "$vscode_home_dir" ] || mkdir -p "$vscode_home_dir"
fi

-log-wait 'Symlinking VSCode settings ...'

ln -sfv "${dotfiles_dir}/vscode/settings.json" \
    "${vscode_home_dir}/User/settings.json"
ln -sfv "${dotfiles_dir}/vscode/keybindings.json" \
    "${vscode_home_dir}/User/keybindings.json"

-log-done "Symlinks were successfully created."

-log-wait 'Installing VSCode extensions ...'

while read -r line
do
  code --install-extension "$line"
done <"${dotfiles_dir}/vscode/extensions.txt"

-log-done 'VSCode extensions were successfully installed.'
