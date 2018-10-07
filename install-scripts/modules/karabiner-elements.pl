# karabiner-elements.pl --- Karabiner Elements installer
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;

if (&is_macos) {
    brew_cask_install('karabiner-elements');

    ln('karabiner-elements/karabiner.json', "${ENV{XDG_CONFIG_HOME}}/karabiner/karabiner.json")
}

1;
