# thunar.bash --- Thunar installer
# author: Seong Yong-ju <sei40kr@gmail.com>

if is_arch; then
    pacman_sync_facade thunar
fi