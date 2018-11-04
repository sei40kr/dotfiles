# desktopservices.pl
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;

if (&is_macos) {

    # Avoid creating .DS_Store files on network or USB volumes
    defaults_write_bool( 'com.apple.desktopservices',
        'DSDontWriteNetworkStores', 1 );
    defaults_write_bool( 'com.apple.desktopservices', 'DSDontWriteUSBStores',
        1 );
}

1;
