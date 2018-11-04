# xorg.pl --- Xorg installer
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;
use FindBin;
use lib "${FindBin::Bin}/install-scripts/lib";
use InstallHelper::Path;

if (&is_arch) {
    pacman_sync('dbus');
    pacman_sync('xorg');
    pacman_sync('xorg-xinit');

    ln( dotfile('xorg/xinitrc'),  "${ENV{HOME}}/.xinitrc" );
    ln( dotfile('xorg/xsession'), "${ENV{HOME}}/.xsession" );
}

1;
