# git.pl --- Git installer
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;

if (&is_macos) {
    brew_install('git');
}
elsif (&is_arch) {
    pacman_sync('git');
}

if ( &is_macos or &is_arch ) {
    ln( 'git/gitconfig', "${ENV{HOME}}/.gitconfig" );
}

1;
