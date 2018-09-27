# docker.pl --- Docker installer
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;

if (&is_macos) {
    brew_cask_install('docker');
    brew_install( 'docker-compose', 'without-docker' );
}
elsif (&is_arch) {
    pacman_sync('docker');
    pacman_sync('docker-compose');

    systemctl_enable('docker.service');
}

1;
