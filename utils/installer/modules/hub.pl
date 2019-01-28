# hub.pl --- hub installer
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;

# TODO Install hub on non-macOS envs
if (&is_macos) {
    brew_install('hub');
}

1;
