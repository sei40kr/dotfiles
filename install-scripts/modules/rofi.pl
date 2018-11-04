# rofi.pl --- Rofi installer
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;
use FindBin;
use lib "${FindBin::Bin}/install-scripts/lib";
use InstallHelper::Path;

if (&is_arch) {
    pacman_sync('rofi');

    ln( dotfile('rofi/config'), "${ENV{XDG_CONFIG_HOME}}/rofi/config" );
}

1;
