# transmission.bash --- Transmission
# author: Seong Yong-ju <sei40kr@gmail.com>

if is_arch; then
    pacman_sync_facade transmission-cli
    pacman_sync_facade transmission-gtk
fi
