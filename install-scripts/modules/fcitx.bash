# fcitx.bash --- fcitx
# author: Seong Yong-ju <sei40kr@gmail.com>

if is_arch; then
    pacman_sync_facade fcitx
    pacman_sync_facade fcitx-gtk2
    pacman_sync_facade fcitx-gtk3
    pacman_sync_facade fcitx-qt4
    pacman_sync_facade fcitx-qt5
    pacman_sync_facade fcitx-configtool
    pacman_sync_facade fcitx-mozc

    ln_facade "${DOTFILES_PATH}/fcitx/conf/fcitx-classic-ui.config" "${XDG_CONFIG_HOME}/fcitx/conf/fcitx-classic-ui.config"
    ln_facade "${DOTFILES_PATH}/fcitx/config" "${XDG_CONFIG_HOME}/fcitx/config"
fi
