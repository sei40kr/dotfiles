# asciinema.pl
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;

if (&is_macos) {
    brew_install('imagemagick');
    brew_install('giflossy');
}
elsif (&is_arch) {
    # TODO Install dependencies on Arch Linux
}

pip3_install('asciinema');

yarn_global_add('asciicast2gif');

1;
