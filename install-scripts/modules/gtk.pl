# gtk.pl --- GTK installer
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;

if (&is_arch) {
    pacman_sync('gtk3');
    pacman_sync('gtk2');

    ln( 'gtk-3.0/settings.ini',
        "${ENV{XDG_CONFIG_HOME}}/gtk-3.0/settings.ini" );
    ln( 'gtk-2.0/gtkrc', "${ENV{XDG_CONFIG_HOME}}/gtk-2.0/gtkrc" );
    ln( 'gtk-2.0/gtkfilechooser.ini',
        "${ENV{XDG_CONFIG_HOME}}/gtk-2.0/gtkfilechooser.ini" );
}

1;
