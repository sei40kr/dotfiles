# fontconfig.pl --- Fontconfig installer
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;

if (&is_arch) {
    pacman_sync('fontconfig');

    ln( "fontconfig/conf.d", "${ENV{XDG_CONFIG_HOME}}/fontconfig/conf.d" );
    ln( "fontconfig/fonts.conf",
        "${ENV{XDG_CONFIG_HOME}}/fontconfig/fonts.conf" );
}

1;
