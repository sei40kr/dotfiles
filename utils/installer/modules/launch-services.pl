# launch-services.pl
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;

if (&is_macos) {

    # Disable the “Are you sure you want to open this application?” dialog
    defaults_write_bool( 'com.apple.LaunchServices', 'LSQuarantine', 0 );
}

1;
