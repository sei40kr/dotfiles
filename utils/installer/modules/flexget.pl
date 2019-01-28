# flexget.pl --- FlexGet installer
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;
use FindBin;
use lib "${FindBin::Bin}/utils/installer/lib";
use InstallHelper::Path;

pip3_install('FlexGet');
pip3_install('transmissionrpc');

ln( dotfile('flexget/config.yml'),
    "${ENV{XDG_CONFIG_HOME}}/flexget/config.yml" );

if (&is_macos) {
    launchctl_load( dotfile('flexget/com.flexget.plist') );
}

1;
