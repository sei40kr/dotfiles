# global.pl --- Source code tag system installer
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;
use FindBin;
use lib "${FindBin::Bin}/utils/installer/lib";
use InstallHelper::Path;

if (&is_macos) {
    brew_install('ctags');
    brew_install('global');
    pip3_install('pygments');
}
else {
    # TODO Install global on other envs
}

ln( dotfile('global/globalrc'), "${ENV{HOME}}/.globalrc" );

1;
