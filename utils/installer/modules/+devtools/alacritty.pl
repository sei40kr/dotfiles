# alacritty.pl --- Alacritty installer
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;
use FindBin;
use lib "${FindBin::Bin}/utils/installer/lib";
use Install::PathResolver;

if (&is_macos) {
    brew_cask_install('alacritty');

    ln( dotfile('alacritty/alacritty.yml'),
        "${ENV{XDG_CONFIG_HOME}}/alacritty/alacritty.yml" );

    defaults_write_bool( 'NSGlobalDomain',
        'CGFontRenderingFontSmoothingDisabled', 0 );
}

1;
