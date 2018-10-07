# macos-tweak-mas.pl --- Mac App Store tweaker
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;

if (&is_macos) {

    # Enable the automatic update check
    defaults_write_bool( 'com.apple.SoftwareUpdate', 'AutomaticCheckEnabled',
        1 );

    # Check for software updates daily, not just once per week
    defaults_write_int( 'com.apple.SoftwareUpdate', 'ScheduleFrequency', 1 );

    # Download newly available updates in background
    defaults_write_int( 'com.apple.SoftwareUpdate', 'AutomaticDownload', 1 );

    # Install System data files & security updates
    defaults_write_int( 'com.apple.SoftwareUpdate', 'CriticalUpdateInstall',
        1 );

    # Turn on app auto-update
    defaults_write_bool( 'com.apple.commerce', 'AutoUpdate', 1 );
}

1;
