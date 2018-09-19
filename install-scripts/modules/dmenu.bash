# dmenu.bash --- dmenu
# author: Seong Yong-ju <sei40kr@gmail.com>

if is_arch; then
    pacman_sync_facade dmenu

    ln_facade "${DOTFILES_PATH}/dmenu/dmenurc" "${HOME}/.dmenurc"
fi
