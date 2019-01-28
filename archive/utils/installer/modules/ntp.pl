# ntp.pl
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;

if (&is_arch) {
    pacman_sync('ntp');
    pacman_sync('networkmanager-dispatcher-ntpd');

    # TODO Run this as root
    # ln('/usr/share/zoneinfo/Asia/Tokyo', '/etc/localtime');

    systemctl_enable('ntpd.service');
}

1;
