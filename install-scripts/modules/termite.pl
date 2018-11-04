# termite.pl --- Termite installer
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;
use FindBin;
use lib "${FindBin::Bin}/install-scripts/lib";
use InstallHelper::Path;

if (&is_arch) {
    pacman_sync('termite');
    pacman_sync('termite-terminfo');

    ln( dotfile('termite/config'), "${ENV{XDG_CONFIG_HOME}}/termite/config" );
}

1;
