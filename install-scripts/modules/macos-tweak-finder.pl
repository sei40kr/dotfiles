# macos-tweak-finder.pl --- Finder tweaker
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;

if (&is_macos) {

    # Finder: disable window animations and Get Info animations
    defaults_write_bool( 'com.apple.finder', 'DisableAllAnimations', 1 );

    # Set Desktop as the default location for new Finder windows
    # For other paths, use `PfLo` and `file:///full/path/here/`
    defaults_write_string( 'com.apple.finder', 'NewWindowTarget', 'PfLo' );
    defaults_write_string( 'com.apple.finder', 'NewWindowTargetPath',
        "file://${ENV{HOME}}" );

    # Show icons for hard drives, servers, and removable media on the desktop
    defaults_write_bool( 'com.apple.finder', 'ShowExternalHardDrivesOnDesktop',
        1 );
    defaults_write_bool( 'com.apple.finder', 'ShowHardDrivesOnDesktop',     1 );
    defaults_write_bool( 'com.apple.finder', 'ShowMountedServersOnDesktop', 1 );
    defaults_write_bool( 'com.apple.finder', 'ShowRemovableMediaOnDesktop', 1 );

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

    # Avoid creating .DS_Store files on network or USB volumes
    defaults_write_bool( 'com.apple.desktopservices',
        'DSDontWriteNetworkStores', 1 );
    defaults_write_bool( 'com.apple.desktopservices', 'DSDontWriteUSBStores',
        1 );

    # Automatically open a new Finder window when a volume is mounted
    defaults_write_bool( 'com.apple.frameworks.diskimages',
        'auto-open-ro-root', 1 );
    defaults_write_bool( 'com.apple.frameworks.diskimages',
        'auto-open-rw-root', 1 );
    defaults_write_bool( 'com.apple.finder', 'OpenWindowForNewRemovableDisk',
        1 );

    # Use list view in all Finder windows by default
    # Four-letter codes for the other view modes: `icnv`, `clmv`, `Flwv`
    defaults_write_string( 'com.apple.finder', 'FXPreferredViewStyle', 'Nlsv' );

    # Disable the warning before emptying the Trash
    defaults_write_bool( 'com.apple.finder', 'WarnOnEmptyTrash', 0 );
}

1;
