# compton.pl --- compton installer
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;
use FindBin;
use lib "${FindBin::Bin}/utils/installer/lib";
use InstallHelper::Path;

if (&is_arch) {
    trizen_sync('compton-no-blur-limit-git');

    ln( dotfile('compton/compton.conf'), "${ENV{XDG_CONFIG_HOME}}/compton.conf" );
    ln( dotfile('compton/comp'),         "${ENV{HOME}}/.local/bin/comp" );
}

1;
