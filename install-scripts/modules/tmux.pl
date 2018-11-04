# tmux.pl --- TMUX installer
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;
use FindBin;
use lib "${FindBin::Bin}/install-scripts/lib";
use InstallHelper::Path;

if (&is_macos) {
    brew_install( 'tmux', 'with-utf8proc' );
    brew_install('reattach-to-user-namespace');
}
elsif (&is_arch) {
    pacman_sync('tmux');
}

if ( &is_macos or &is_arch ) {
    git_clone(
        'https://github.com/tmux-plugins/tpm.git',
        "${ENV{HOME}}/.tmux/plugins/tpm"
    );

    ln( dotfile('tmux/tmux.conf'), "${ENV{HOME}}/.tmux.conf" );
}

1;
