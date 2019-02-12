# direnv.pl --- direnv installer
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;

if (&is_macos) {
    brew_install('direnv');
}
elsif (&is_arch) {
    pacman_sync('make');

    trizen_sync('direnv');
}

1;
