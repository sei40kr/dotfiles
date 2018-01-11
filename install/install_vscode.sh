#!/bin/sh

# install_vscode.sh
# @author Seong Yong-ju ( @sei40kr )

DOTFILES_PREFIX="${HOME}/dotfiles"

if [ -d "${HOME}/.config/Code/User" ]
then
  VSCODE_SETTING_PREFIX="${HOME}/.config/Code/User"
elif [ -d "${HOME}/Library/Application Support/Code/User" ]
then
  VSCODE_SETTING_PREFIX="${HOME}/Library/Application Support/Code/User"
else
  echo "Warning: Could not found VSCode" >&2
fi

# Create Symlinks
# ---------------

ln -sf "${DOTFILES_PREFIX}/vscode/keybindings.json" \
    "${VSCODE_SETTING_PREFIX}/keybindings.json"
ln -sf "${DOTFILES_PREFIX}/vscode/settings.json" \
    "${VSCODE_SETTING_PREFIX}/settings.json"

# Install Extensions
# ------------------

while read -r line
do
  code --install-extension "$line"
done <"${DOTFILES_PREFIX}/vscode/extensions.txt"

