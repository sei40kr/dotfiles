# vim.pl --- Vim installer
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;
use FindBin;
use lib "${FindBin::Bin}/utils/installer/lib";
use Install::PathResolver;

if (&is_macos) {
    brew_install('vim');
}
elsif (&is_arch) {
    pacman_sync('vim');
}

ln( dotfile('vim/vimrc'), "${ENV{HOME}}/.vimrc" );

1;
