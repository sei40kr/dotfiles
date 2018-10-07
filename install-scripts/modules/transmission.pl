# transmission.pl --- Transmission installer
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;

if (&is_macos) {
    brew_cask_install('transmission');

    # Use `~/Documents/Torrents` to store incomplete downloads
    defaults_write_bool( 'org.m0k.transmission', 'UseIncompleteDownloadFolder',
        1 );
    defaults_write_string( 'org.m0k.transmission', 'IncompleteDownloadFolder',
        "${ENV{HOME}}/Documents/Torrents" );

    # Use `~/Downloads` to store completed downloads
    defaults_write_bool( 'org.m0k.transmission', 'DownloadLocationConstant',
        1 );

    # Don’t prompt for confirmation before downloading
    defaults_write_bool( 'org.m0k.transmission', 'DownloadAsk',   0 );
    defaults_write_bool( 'org.m0k.transmission', 'MagnetOpenAsk', 0 );

    # Don’t prompt for confirmation before removing non-downloading active transfers
    defaults_write_bool( 'org.m0k.transmission', 'CheckRemoveDownloading', 1 );

    # Trash original torrent files
    defaults_write_bool( 'org.m0k.transmission', 'DeleteOriginalTorrent', 1 );

    # Hide the donate message
    defaults_write_bool( 'org.m0k.transmission', 'WarningDonate', 0 );

    # Hide the legal disclaimer
    defaults_write_bool( 'org.m0k.transmission', 'WarningLegal', 0 );

    # Randomize port on launch
    defaults_write_bool( 'org.m0k.transmission', 'RandomPort', 1 );
}
elsif (&is_arch) {
    pacman_sync('transmission-cli');
    pacman_sync('transmission-gtk');
}

1;
