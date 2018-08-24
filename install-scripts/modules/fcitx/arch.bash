# arch.bash --- fcitx installer for ArchLinux
# author: Seong Yong-ju <sei40kr@gmail.com>

require_facades pacman

pacman_sync_pkg fcitx
pacman_sync_pkg fcitx-gtk2
pacman_sync_pkg fcitx-gtk3
pacman_sync_pkg fcitx-qt4
pacman_sync_pkg fcitx-qt5
pacman_sync_pkg fcitx-configtool
pacman_sync_pkg fcitx-mozc
