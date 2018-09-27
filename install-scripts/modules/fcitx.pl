# fcitx.pl ---
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;

if (&is_arch) {
    pacman_sync('fcitx');
    pacman_sync('fcitx-gtk3');
    pacman_sync('fcitx-gtk2');
    pacman_sync('fcitx-qt5');
    pacman_sync('fcitx-qt4');
    pacman_sync('fcitx-configtool');
    pacman_sync('fcitx-mozc');

    ln( 'fcitx/conf/fcitx-classic-ui.config',
        "${ENV{XDG_CONFIG_HOME}}/fcitx/conf/fcitx-classic-ui.config" );
    ln( 'fcitx/config', "${ENV{XDG_CONFIG_HOME}}/fcitx/config" );
}

1;
