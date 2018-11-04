# flexget.pl --- FlexGet installer
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;
use FindBin;
use lib "${FindBin::Bin}/install-scripts/lib";
use InstallHelper::Path;

pip3_install('FlexGet');
pip3_install('transmissionrpc');

ln( dotfile('flexget/config.yml'),
    "${ENV{XDG_CONFIG_HOME}}/flexget/config.yml" );

if (&is_macos) {
    ln( dotfile('flexget/com.flexget.plist'),
        "${ENV{HOME}}/Library/LaunchAgents/com.flexget.plist" );
}

1;
