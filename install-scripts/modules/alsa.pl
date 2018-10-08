# alsa.pl --- ALSA installer
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;

if (&is_arch) {
    pacman_sync('alsa-firmware');
    pacman_sync('alsa-utils');
    pacman_sync('pulseaudio-alsa');
}

1;
