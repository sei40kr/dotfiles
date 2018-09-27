# xorg.pl --- Xorg installer
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;

if (&is_arch) {
    pacman_sync('dbus');
    pacman_sync('xorg');
    pacman_sync('xorg-xinit');

    ln('xorg/xinitrc', "${ENV{HOME}}/.xinitrc");
    ln('xorg/xsession', "${ENV{HOME}}/.xsession");
}

1;
