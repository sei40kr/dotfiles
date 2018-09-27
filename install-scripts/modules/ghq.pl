# ghq.pl --- ghq installer
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;

if (&is_macos) {
    brew_install('ghq');
}
elsif (&is_arch) {
    trizen_sync('ghq');
}

1;
