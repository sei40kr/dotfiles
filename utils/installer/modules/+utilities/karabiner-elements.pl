# karabiner-elements.pl --- Karabiner Elements installer
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;
use FindBin;
use lib "${FindBin::Bin}/utils/installer/lib";
use Install::PathResolver;

if (&is_macos) {
    brew_cask_install('karabiner-elements');

    ln(
        dotfile('karabiner-elements/karabiner.json'),
        "${ENV{XDG_CONFIG_HOME}}/karabiner/karabiner.json"
    );
}

1;
