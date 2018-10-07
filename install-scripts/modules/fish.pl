# fish.pl --- fish shell installer
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;

if (&is_macos) {
    brew_install('fish');
    brew_install('fzf');
}
elsif (&is_arch) {
    pacman_sync('fish');
    pacman_sync('fzf');
}

if ( &is_macos or &is_arch ) {
    ln( 'fish/bash_profile', "${ENV{HOME}}/.bash_profile" );
    ln( 'fish/bashrc',       "${ENV{HOME}}/.bashrc" );

    curl('https://git.io/fisher', "${ENV{XDG_CONFIG_HOME}}/fish/functions/fisher.fish");

    ln( 'fish/config.fish', "${ENV{XDG_CONFIG_HOME}}/fish/config.fish" );
}

1;
