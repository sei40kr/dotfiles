# termite.bash --- Termite installer
# author: Seong Yong-ju <sei40kr@gmail.com>

if is_arch; then
    pacman_sync_facade termite
    pacman_sync_facade termite-terminfo

    ln_facade "${DOTFILES_PATH}/termite/config" "${XDG_CONFIG_HOME}/termite/config"
fi
