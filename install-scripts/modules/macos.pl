# macos.pl --- macOS tweaker
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;

if (&is_macos) {

    # Always show scrollbars
    defaults_write_string( 'NSGlobalDomain', 'AppleShowScrollBars', 'Always' );

    # Disable the over-the-top focus ring animation
    defaults_write_bool( 'NSGlobalDomain', 'NSUseAnimatedFocusRing', 0 );

    # Increase window resize speed for Cocoa applications
    defaults_write_float( 'NSGlobalDomain', 'NSWindowResizeTime', 0.001 );

    # Expand save panel by default
    defaults_write_bool( 'NSGlobalDomain',
        'NSNavPanelExpandedStateForSaveMode', 1 );
    defaults_write_bool( 'NSGlobalDomain',
        'NSNavPanelExpandedStateForSaveMode2', 1 );

    # Expand print panel by default
    defaults_write_bool( 'NSGlobalDomain', 'PMPrintingExpandedStateForPrint',
        1 );
    defaults_write_bool( 'NSGlobalDomain', 'PMPrintingExpandedStateForPrint2',
        1 );

    # Save to disk (not to iCloud) by default
    defaults_write_bool( 'NSGlobalDomain', 'NSDocumentSaveNewDocumentsToCloud',
        0 );

    # Automatically quit printer app once the print jobs complete
    defaults_write_bool( 'com.apple.print.PrintingPrefs',
        'Quit When Finished', 1 );

    # Disable the “Are you sure you want to open this application?” dialog
    defaults_write_bool( 'com.apple.LaunchServices', 'LSQuarantine', 0 );

    # Disable Resume system-wide
    defaults_write_bool( 'com.apple.systempreferences',
        'NSQuitAlwaysKeepsWindows', 0 );

    # Disable automatic termination of inactive apps
    defaults_write_bool( 'NSGlobalDomain', 'NSDisableAutomaticTermination', 1 );

    # Disable the crash reporter
    defaults_write_string( 'com.apple.CrashReporter', 'DialogType', 'none' );

    # Disable automatic capitalization as it’s annoying when typing code
    defaults_write_bool( 'NSGlobalDomain', 'NSAutomaticCapitalizationEnabled',
        0 );

    # Disable smart dashes as they’re annoying when typing code
    defaults_write_bool( 'NSGlobalDomain',
        'NSAutomaticDashSubstitutionEnabled', 0 );

    # Disable automatic period substitution as it’s annoying when typing code
    defaults_write_bool( 'NSGlobalDomain',
        'NSAutomaticPeriodSubstitutionEnabled', 0 );

    # Disable smart quotes as they’re annoying when typing code
    defaults_write_bool( 'NSGlobalDomain',
        'NSAutomaticQuoteSubstitutionEnabled', 0 );

    # Disable auto-correct
    defaults_write_bool( 'NSGlobalDomain',
        'NSAutomaticSpellingCorrectionEnabled', 0 );

    # Enable full keyboard access for all controls
    # (e.g. enable Tab in modal dialogs)
    defaults_write_int( 'NSGlobalDomain', 'AppleKeyboardUIMode', 3 );

    # Set a blazingly fast keyboard repeat rate
    defaults_write_int( 'NSGlobalDomain', 'KeyRepeat',        1 );
    defaults_write_int( 'NSGlobalDomain', 'InitialKeyRepeat', 10 );

    # Save screenshots to the desktop
    defaults_write_string( 'com.apple.screencapture', 'location',
        "${ENV{HOME}}/Desktop" );

    # Save screenshots in PNG format (other options: BMP, GIF, JPG, PDF, TIFF)
    defaults_write_string( 'com.apple.screencapture', 'type', 'png' );

    # Disable shadow in screenshots
    defaults_write_bool( 'com.apple.screencapture', 'disable-shadow', 1 );

    # Disable disk image verification
    defaults_write_bool( 'com.apple.frameworks.diskimages', 'skip-verify', 1 );
    defaults_write_bool( 'com.apple.frameworks.diskimages',
        'skip-verify-locked', 1 );
    defaults_write_bool( 'com.apple.frameworks.diskimages',
        'skip-verify-remote', 1 );

## Time Machine

   # Prevent Time Machine from prompting to use new hard drives as backup volume
    defaults_write_bool( 'com.apple.TimeMachine',
        'DoNotOfferNewDisksForBackup', 1 );

## Activity Monitor

    # Show the main window when launching Activity Monitor
    defaults_write_bool( 'com.apple.ActivityMonitor', 'OpenMainWindow', 1 );

    # Visualize CPU usage in the Activity Monitor Dock icon
    defaults_write_int( 'com.apple.ActivityMonitor', 'IconType', 5 );

    # Show all processes in Activity Monitor
    defaults_write_int( 'com.apple.ActivityMonitor', 'ShowCategory', 0 );

    # Sort Activity Monitor results by CPU usage
    defaults_write_string( 'com.apple.ActivityMonitor', 'SortColumn',
        "CPUUsage" );
    defaults_write_int( 'com.apple.ActivityMonitor', 'SortDirection', 0 );
}

1;
