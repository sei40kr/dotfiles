# chunkwm.pl --- chunkwm
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;
use FindBin;
use lib "${FindBin::Bin}/utils/installer/lib";
use Install::PathResolver;

if (&is_macos) {
    brew_tap('koekeishiya/formulae');

    # install chunkwm
    brew_install('chunkwm');
    ln( dotfile('chunkwm/chunkwmrc'), "${ENV{HOME}}/.chunkwmrc" );

    # TODO enable chunkwm service

    # install skhd
    brew_install('skhd');
    ln( dotfile('chunkwm/skhdrc'), "${ENV{HOME}}/.skhdrc" );

    # TODO enable skhd service
}

1;
