# termite.pl --- Termite installer
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;

if (&is_arch) {
    pacman_sync('termite');
    pacman_sync('termite-terminfo');

    ln( 'termite/config', "${ENV{XDG_CONFIG_HOME}}/termite/config" );
}

1;
