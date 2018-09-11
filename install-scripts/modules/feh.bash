# feh.bash --- feh installer
# author: Seong Yong-ju <sei40kr@gmail.com>

if is_arch; then
    pacman_sync_facade feh

    ln_facade "${DOTFILES_PATH}/feh/fehbg" "${HOME}/.fehbg"
    ln_facade "${DOTFILES_PATH}/feh/arch-01-1920x1080.png" "${XDG_DATA_HOME}/backgrounds/arch-01-1920x1080.png"
fi
