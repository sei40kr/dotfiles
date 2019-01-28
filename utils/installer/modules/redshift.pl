# redshift.pl --- Redshift
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;
use FindBin;
use lib "${FindBin::Bin}/utils/installer/lib";
use InstallHelper::Path;

if (&is_arch) {
    pacman_sync('redshift');

    ln( dotfile('redshift/redshift.conf'),
        "${ENV{XDG_CONFIG_HOME}}/redshift/redshift.conf" );
    ln( dotfile('redshift/hooks/brightness.sh'),
        "${ENV{XDG_CONFIG_HOME}}/redshift/hooks/brightness.sh" );

    systemctl_enable_user('redshift.service');
}

1;
