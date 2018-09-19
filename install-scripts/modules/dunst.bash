# dunst.bash --- Dunst
# author: Seong Yong-ju <sei40kr@gmail.com>

if is_arch; then
    pacman_sync_facade dunst

    ln_facade "${DOTFILES_PATH}/dunst/dunstrc" "${XDG_CONFIG_HOME}/dunst/dunstrc"
fi
