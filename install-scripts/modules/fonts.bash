# fonts.bash --- Fonts
# author: Seong Yong-ju <sei40kr@gmail.com>

if is_arch; then
    pacman_sync_facade nerd-fonts-fira-code
    pacman_sync_facade noto-fonts
    pacman_sync_facade noto-fonts-cjk
    pacman_sync_facade terminus-font
    pacman_sync_facade ttf-liberation
fi
