# tig.pl --- tig installer
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;
use FindBin;
use lib "${FindBin::Bin}/utils/installer/lib";
use Install::PathResolver;

if (&is_macos) {
    brew_install('tig');
} else {
    # TODO Install tig on other envs
}

ln( dotfile('tig/tigrc'), "${ENV{HOME}}/.tigrc" );

1;
