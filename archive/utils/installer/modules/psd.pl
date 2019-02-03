# psd.pl --- Profile-sync-daemon installer
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;
use FindBin;
use lib "${FindBin::Bin}/utils/installer/lib";
use Install::PathResolver;

if (&is_arch) {
    trizen_sync('profile-sync-daemon');

    ln( dotfile('psd/psd.conf'), "${ENV{XDG_CONFIG_HOME}}/psd/psd.conf" );

    systemctl_enable_user('psd.service');
}

1;
