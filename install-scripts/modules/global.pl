# global.pl --- Source code tag system installer
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;

if (&is_macos) {
    brew_install('global');
    pip3_install('pygments');
}
else {
    # TODO Install global on other envs
}

1;
