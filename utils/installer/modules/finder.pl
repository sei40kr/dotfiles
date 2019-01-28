# finder.pl
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;

if (&is_macos) {

    # Finder: disable window animations and Get Info animations
    defaults_write_bool( 'com.apple.finder', 'DisableAllAnimations', 1 );

    # Finder: show hidden files by default
    defaults_write_bool( 'com.apple.finder', 'AppleShowAllFiles', 1 );

    # Finder: show all filename extensions
    defaults_write_bool( 'NSGlobalDomain', 'AppleShowAllExtensions', 1 );

    # Finder: show status bar
    defaults_write_bool( 'com.apple.finder', 'ShowStatusBar', 1 );

    # Finder: show path bar
    defaults_write_bool( 'com.apple.finder', 'ShowPathbar', 1 );

    # Display full POSIX path as Finder window title
    defaults_write_bool( 'com.apple.finder', '_FXShowPosixPathInTitle', 1 );

    # Keep folders on top when sorting by name
    defaults_write_bool( 'com.apple.finder', '_FXSortFoldersFirst', 1 );

    # When performing a search, search the current folder by default
    defaults_write_string( 'com.apple.finder', 'FXDefaultSearchScope', "SCcf" );

    # Disable the warning when changing a file extension
    defaults_write_bool( 'com.apple.finder', 'FXEnableExtensionChangeWarning',
        0 );

    # Use list view in all Finder windows by default
    # Four-letter codes for the other view modes: `icnv`, `clmv`, `Flwv`
    defaults_write_string( 'com.apple.finder', 'FXPreferredViewStyle', 'Nlsv' );
}

1;
