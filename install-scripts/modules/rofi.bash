# rofi.bash --- Rofi installer
# author: Seong Yong-ju <sei40kr@gmail.com>

if is_arch; then
    pacman_sync_facade rofi

    ln_facade "${DOTFILES_PATH}/rofi/config" "${XDG_CONFIG_HOME}/rofi/config"
fi
