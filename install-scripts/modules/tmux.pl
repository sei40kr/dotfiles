# tmux.pl --- TMUX installer
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;

if (&is_macos) {
    brew_install( 'tmux', 'with-utf8proc' );
    brew_install('reattach-to-user-namespace');
}
elsif (&is_arch) {
    pacman_sync('tmux');
}

if ( &is_macos or &is_arch ) {
    ln( 'tmux/tmux.conf',      "${ENV{HOME}}/.tmux.conf" );
    ln( 'tmux/tmux-cpu-usage', "${ENV{HOME}}/.local/bin/tmux-cpu-usage" );
    ln( 'tmux/tmux-mem-usage', "${ENV{HOME}}/.local/bin/tmux-mem-usage" );
}

1;
