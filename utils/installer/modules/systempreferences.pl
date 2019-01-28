# systempreferences.pl
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;

if (&is_macos) {

    # Disable Resume system-wide
    defaults_write_bool( 'com.apple.systempreferences',
        'NSQuitAlwaysKeepsWindows', 0 );
}

1;
