# google-chrome.pl --- Google Chrome installer
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;

if (&is_macos) {
    brew_cask_install('google-chrome');

    brew_cask_install('google-hangouts');
}
elsif (&is_arch) {
    trizen_sync('google-chrome');
}

1;
