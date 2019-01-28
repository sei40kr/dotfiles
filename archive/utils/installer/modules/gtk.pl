# gtk.pl --- GTK installer
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;
use FindBin;
use lib "${FindBin::Bin}/utils/installer/lib";
use InstallHelper::Path;

if (&is_arch) {
    pacman_sync('gtk3');
    pacman_sync('gtk2');

    ln( dotfile('gtk-3.0/settings.ini'),
        "${ENV{XDG_CONFIG_HOME}}/gtk-3.0/settings.ini" );
    ln( dotfile('gtk-2.0/gtkrc'), "${ENV{XDG_CONFIG_HOME}}/gtk-2.0/gtkrc" );
    ln( dotfile('gtk-2.0/gtkfilechooser.ini'),
        "${ENV{XDG_CONFIG_HOME}}/gtk-2.0/gtkfilechooser.ini" );
}

1;
