# networkmanager.pl --- NetworkManager installer
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;

if (&is_arch) {
    pacman_sync('networkmanager');
    pacman_sync('dialog');
}

1;
