# vim.pl --- Vim installer
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;

if (&is_macos) {
    brew_install('vim');
}
elsif (&is_arch) {
    pacman_sync('vim');
}

ln( 'vim/vimrc', "${ENV{HOME}}/.vimrc" );

1;
