# macos-tweak-qtp.pl --- QuickTime Player tweaker
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;

if (&is_macos) {

    # Auto-play videos when opened with QuickTime Player
    defaults_write_bool( 'com.apple.QuickTimePlayerX', 'MGPlayMovieOnOpen', 1 );
}

1;
