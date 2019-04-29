# yoink.pl --- Yoink installer
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;

if (&is_macos) {
    mas_install('457622435');
}

1;
