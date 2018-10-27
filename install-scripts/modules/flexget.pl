# flexget.pl --- FlexGet installer
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;

pip3_install('FlexGet');
pip3_install('transmissionrpc');

ln( 'flexget/config.yml', "${ENV{XDG_CONFIG_HOME}}/flexget/config.yml" );

if (&is_macos) {
    ln( 'flexget/com.flexget.plist',
        "${ENV{HOME}}/Library/LaunchAgents/com.flexget.plist" );
}

1;
