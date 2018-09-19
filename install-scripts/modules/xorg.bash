# xorg.bash --- Xorg
# author: Seong Yong-ju <sei40kr@gmail.com>

if is_arch; then
    pacman_sync_facade dbus
    pacman_sync_facade xorg
    pacman_sync_facade xorg-xinit

    ln_facade "${DOTFILES_PATH}/xorg/xinitrc" "${HOME}/.xinitrc"
    ln_facade "${DOTFILES_PATH}/xorg/xsession" "${HOME}/.xsession"
fi
