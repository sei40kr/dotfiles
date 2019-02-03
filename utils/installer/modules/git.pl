# git.pl --- Git installer
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;
use FindBin;
use lib "${FindBin::Bin}/utils/installer/lib";
use Install::PathResolver;

# TODO Install git-flow on non-macOS envs
if (&is_macos) {
    brew_install('git');
    brew_install('git-flow');
}
elsif (&is_arch) {
    pacman_sync('git');
}

if ( &is_macos or &is_arch ) {
    ln( dotfile('git/gitconfig'), "${ENV{HOME}}/.gitconfig" );
    ln( dotfile('git/gitignore'), "${ENV{HOME}}/.gitignore_global" );
}

1;
