# compton.pl --- compton installer
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;

if (&is_arch) {
    trizen_sync('compton-no-blur-limit-git');

    ln( 'compton/compton.conf', "${ENV{XDG_CONFIG_HOME}}/compton.conf" );
    ln( 'compton/comp',         "${ENV{HOME}}/.local/bin/comp" );
}

1;
