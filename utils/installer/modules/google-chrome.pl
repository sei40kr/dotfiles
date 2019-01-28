# google-chrome.pl --- Google Chrome installer
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;

if (&is_macos) {
    brew_cask_install('google-chrome');

    # Disable the all too sensitive backswipe on trackpads
    defaults_write_bool( 'com.google.Chrome',
        'AppleEnableSwipeNavigateWithScrolls', 0 );

    # Disable the all too sensitive backswipe on Magic Mouse
    defaults_write_bool( 'com.google.Chrome',
        'AppleEnableMouseSwipeNavigateWithScrolls', 0 );

    # Use the system-native print preview dialog
    defaults_write_bool( 'com.google.Chrome', 'DisablePrintPreview', 1 );

    # Expand the print dialog by default
    defaults_write_bool( 'com.google.Chrome',
        'PMPrintingExpandedStateForPrint2', 1 );
}
elsif (&is_arch) {
    trizen_sync('google-chrome');
}

1;
