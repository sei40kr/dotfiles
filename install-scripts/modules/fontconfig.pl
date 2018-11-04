# fontconfig.pl --- Fontconfig installer
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;
use FindBin;
use lib "${FindBin::Bin}/install-scripts/lib";
use InstallHelper::Path;

if (&is_arch) {
    pacman_sync('fontconfig');

    ln( dotfile('fontconfig/conf.d'), "${ENV{XDG_CONFIG_HOME}}/fontconfig/conf.d" );
    ln( dotfile('fontconfig/fonts.conf'),
        "${ENV{XDG_CONFIG_HOME}}/fontconfig/fonts.conf" );
}

1;
