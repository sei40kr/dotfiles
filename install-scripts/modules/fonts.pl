# fonts.pl --- Fonts installer
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;

if (&is_macos) {
    brew_tap('homebrew/cask-fonts');
    brew_cask_install('font-source-han-code-jp');
    brew_cask_install('font-terminus');
} elsif (&is_arch) {
    pacman_sync('noto-fonts');
    pacman_sync('noto-fonts-cjk');
    pacman_sync('noto-fonts-emoji');
    pacman_sync('terminus-font');
    pacman_sync('ttf-liberation');

    trizen_sync('nerd-fonts-fira-code');
}

1;
