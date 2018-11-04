# ns-global-domain.pl
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

    # Expand print panel by default
    defaults_write_bool( 'NSGlobalDomain', 'PMPrintingExpandedStateForPrint',
        1 );

    # Save to disk (not to iCloud) by default
    defaults_write_bool( 'NSGlobalDomain', 'NSDocumentSaveNewDocumentsToCloud',
        0 );

    # Disable automatic termination of inactive apps
    defaults_write_bool( 'NSGlobalDomain', 'NSDisableAutomaticTermination', 1 );

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
}

1;
