# psd.pl --- Profile-sync-daemon installer
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;

if (&is_arch) {
    trizen_sync('profile-sync-daemon');

    ln( 'psd/psd.conf', "${ENV{XDG_CONFIG_HOME}}/psd/psd.conf" );

    systemctl_enable_user('psd.service');
}

1;
