# macos-tweak-dock.pl --- Dock tweaker
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;

if (&is_macos) {
    # Enable highlight hover effect for the grid view of a stack (Dock)
    defaults_write_bool( 'com.apple.dock', 'mouse-over-hilite-stack', 1 );

    # Set the icon size of Dock items to 64 pixels
    defaults_write_int( 'com.apple.dock', 'tilesize', 64 );

    # Change minimize/maximize window effect
    defaults_write_string( 'com.apple.dock', 'mineffect', "scale" );

    # Minimize windows into their application’s icon
    defaults_write_bool( 'com.apple.dock', 'minimize-to-application', 1 );

    # Show indicator lights for open applications in the Dock
    defaults_write_bool( 'com.apple.dock', 'show-process-indicators', 1 );

    # Don’t animate opening applications from the Dock
    defaults_write_bool( 'com.apple.dock', 'launchanim', 0 );

    # Speed up Mission Control animations
    defaults_write_float( 'com.apple.dock', 'expose-animation-duration', 0.1 );

    # Don’t group windows by application in Mission Control
    # (i.e. use the old Exposé behavior instead)
    # Disable Dashboard
    defaults_write_bool( 'com.apple.dashboard', 'mcx-disabled', 1 );

    # Don’t automatically rearrange Spaces based on most recent use
    defaults_write_bool( 'com.apple.dock', 'mru-spaces', 0 );

    # Remove the auto-hiding Dock delay
    defaults_write_float( 'com.apple.dock', 'autohide-delay', 0 );

    # Remove the animation when hiding/showing the Dock
    defaults_write_float( 'com.apple.dock', 'autohide-time-modifier', 0 );

    # Automatically hide and show the Dock
    defaults_write_bool( 'com.apple.dock', 'autohide', 1 );

    # Make Dock icons of hidden applications translucent
    defaults_write_bool( 'com.apple.dock', 'showhidden', 1 );

    # Don’t show recent applications in Dock
    defaults_write_bool( 'com.apple.dock', 'show-recents', 0 );

    # Disable the Launchpad gesture (pinch with thumb and three fingers)
    defaults_write_int( 'com.apple.dock', 'showLaunchpadGestureEnabled', 0 );

    # Hot corners
    # Possible values:
    #  0: no-op
    #  2: Mission Control
    #  3: Show application windows
    #  4: Desktop
    #  5: Start screen saver
    #  6: Disable screen saver
    #  7: Dashboard
    # 10: Put display to sleep
    # 11: Launchpad
    # 12: Notification Center
    # Top left screen corner → Mission Control
    defaults_write_int( 'com.apple.dock', 'wvous-tl-corner',   0 );
    defaults_write_int( 'com.apple.dock', 'wvous-tl-modifier', 0 );

    # Top right screen corner → Desktop
    defaults_write_int( 'com.apple.dock', 'wvous-tr-corner',   0 );
    defaults_write_int( 'com.apple.dock', 'wvous-tr-modifier', 0 );

    # Bottom left screen corner → Start screen saver
    defaults_write_int( 'com.apple.dock', 'wvous-bl-corner',   0 );
    defaults_write_int( 'com.apple.dock', 'wvous-bl-modifier', 0 );
}

1;
