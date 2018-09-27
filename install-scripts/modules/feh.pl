# feh.pl --- feh installer
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;

if (&is_arch) {
    pacman_sync('feh');

    ln( 'feh/fehbg', "${ENV{HOME}}/.fehbg" );
    ln( 'feh/arch-01-1920x1080.png',
        "${ENV{XDG_DATA_HOME}}/backgrounds/arch-01-1920x1080.png" );
}

1;
