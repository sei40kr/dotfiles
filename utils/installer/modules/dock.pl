# dock.pl
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;

if (&is_macos) {

    # Set the icon size of Dock items to 64 pixels
    defaults_write_int( 'com.apple.dock', 'tilesize', 64 );

    # Change minimize/maximize window effect
    defaults_write_string( 'com.apple.dock', 'mineffect', "scale" );

    # Minimize windows into their application’s icon
    defaults_write_bool( 'com.apple.dock', 'minimize-to-application', 1 );

    # Don’t animate opening applications from the Dock
    defaults_write_bool( 'com.apple.dock', 'launchanim', 0 );

    # Speed up Mission Control animations
    defaults_write_float( 'com.apple.dock', 'expose-animation-duration', 0.0 );

    # Don’t automatically rearrange Spaces based on most recent use
    defaults_write_bool( 'com.apple.dock', 'mru-spaces', 0 );

    # Don’t show recent applications in Dock
    defaults_write_bool( 'com.apple.dock', 'show-recents', 0 );

    # Disable the Launchpad gesture (pinch with thumb and three fingers)
    defaults_write_int( 'com.apple.dock', 'showLaunchpadGestureEnabled', 0 );

    # Disable hot corners
    defaults_write_int( 'com.apple.dock', 'wvous-tl-corner',   0 );
    defaults_write_int( 'com.apple.dock', 'wvous-tl-modifier', 0 );
    defaults_write_int( 'com.apple.dock', 'wvous-tr-corner',   0 );
    defaults_write_int( 'com.apple.dock', 'wvous-tr-modifier', 0 );
    defaults_write_int( 'com.apple.dock', 'wvous-bl-corner',   0 );
    defaults_write_int( 'com.apple.dock', 'wvous-bl-modifier', 0 );
}

1;
