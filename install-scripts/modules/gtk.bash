# gtk.bash --- GTK
# author: Seong Yong-ju <sei40kr@gmail.com>

if is_arch; then
    pacman_sync_facade gtk2

    ln_facade "${DOTFILES_PATH}/gtk-2.0/gtkrc" "${XDG_CONFIG_HOME}/gtk-2.0/gtkrc"
    ln_facade "${DOTFILES_PATH}/gtk-2.0/gtkfilechooser.ini" "${XDG_CONFIG_HOME}/gtk-2.0/gtkfilechooser.ini"

    pacman_sync_facade gtk3

    ln_facade "${DOTFILES_PATH}/gtk-3.0/settings.ini" "${XDG_CONFIG_HOME}/gtk-3.0/settings.ini"
fi
