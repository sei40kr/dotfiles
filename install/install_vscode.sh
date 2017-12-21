#!/bin/sh

# install_vscode.sh
# @author Seong Yong-ju ( @sei40kr )

DOTFILES_PREFIX="${HOME}/dotfiles"
VSCODE_SETTING_PREFIX="${HOME}/.config/Code/User"

# Create Symlinks
# ---------------

ln -sf "${DOTFILES_PREFIX}/.config/Code/User/keybindings.json" \
    "${VSCODE_SETTING_PREFIX}/keybindings.json"
ln -sf "${DOTFILES_PREFIX}/.config/Code/User/settings.json" \
    "${VSCODE_SETTING_PREFIX}/settings.json"

# Install Extensions
# ------------------

while read -r line
do
  code --install-extension "$line"
done <"${DOTFILES_PREFIX}/.config/Code/User/extensions.txt"

