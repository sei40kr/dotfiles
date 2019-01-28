# google-japanese-ime.pl --- Google Japanese IME installer
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;

if (&is_macos) {
    brew_cask_install('google-japanese-ime');
}

1;
