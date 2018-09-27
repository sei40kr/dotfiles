# dunst.pl --- Dunst installer
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;

if (&is_arch) {
    pacman_sync('dunst');

    ln('dunst/dunstrc', "${ENV{XDG_CONFIG_HOME}}/dunst/dunstrc");
}

1;
