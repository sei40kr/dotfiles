# networkmanager.bash --- NewtorkManager
# author: Seong Yong-ju <sei40kr@gmail.com>

if is_arch; then
    pacman_sync_facade dialog
    pacman_sync_facade networkmanager
fi
