# time-machine.pl
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;

if (&is_macos) {

   # Prevent Time Machine from prompting to use new hard drives as backup volume
    defaults_write_bool( 'com.apple.TimeMachine',
        'DoNotOfferNewDisksForBackup', 1 );
}

1;
