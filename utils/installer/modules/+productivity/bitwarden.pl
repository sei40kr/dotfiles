# bitwarden.pl --- Bitwarden installer
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;

if (&is_macos) {
    mas_install('1352778147');
}

1;
