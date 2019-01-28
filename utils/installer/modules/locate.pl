# locate.pl
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;

if (&is_macos) {
    sudo_launchctl_load('/System/Library/LaunchDaemons/com.apple.locate.plist');
}

1;
