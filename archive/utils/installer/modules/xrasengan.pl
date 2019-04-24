# xrasengan.pl --- xrasengan
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;
use FindBin;
use lib "${FindBin::Bin}/utils/installer/lib";

if (&is_arch) {
    pacman_sync('arandr');
}

1;
