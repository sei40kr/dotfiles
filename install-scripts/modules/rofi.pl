# rofi.pl --- Rofi installer
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;

if (&is_arch) {
    pacman_sync('rofi');

    ln( 'rofi/config', "${ENV{XDG_CONFIG_HOME}}/rofi/config" );
}

1;
