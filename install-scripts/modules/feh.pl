# feh.pl --- feh installer
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;
use FindBin;
use lib "${FindBin::Bin}/install-scripts/lib";
use InstallHelper::Path;

if (&is_arch) {
    pacman_sync('feh');

    ln( dotfile('feh/fehbg'), "${ENV{HOME}}/.fehbg" );
    ln( dotfile('feh/arch-01-1920x1080.png'),
        "${ENV{XDG_DATA_HOME}}/backgrounds/arch-01-1920x1080.png" );
}

1;
