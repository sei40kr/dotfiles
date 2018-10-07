# macos-photos.pl --- Mac App Store tweaker
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;

if (&is_macos) {

    # Prevent Photos from opening automatically when devices are plugged in
    defaults_write_bool( 'com.apple.ImageCapture', 'disableHotPlug', 1 );
}

1;
