# transmission.pl --- Transmission installer
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;

if (&is_macos) {
    brew_cask_install('transmission');
}
elsif (&is_arch) {
    pacman_sync('transmission-cli');
    pacman_sync('transmission-gtk');
}

1;
