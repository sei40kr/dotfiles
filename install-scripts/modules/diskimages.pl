# diskimages.pl
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;

if (&is_macos) {

    # Disable disk image verification
    defaults_write_bool( 'com.apple.frameworks.diskimages', 'skip-verify', 1 );
    defaults_write_bool( 'com.apple.frameworks.diskimages',
        'skip-verify-locked', 1 );
    defaults_write_bool( 'com.apple.frameworks.diskimages',
        'skip-verify-remote', 1 );
}

1;
