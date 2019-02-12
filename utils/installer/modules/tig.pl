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
}
elsif (&is_arch) {
    pacman_sync('tig');
}

if ( &is_macos || &is_arch ) {
    ln( dotfile('tig/tigrc'), "${ENV{HOME}}/.tigrc" );
}

1;
