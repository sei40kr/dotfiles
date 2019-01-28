# crash-reporter.pl
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;

if (&is_macos) {

    # Disable the crash reporter
    defaults_write_string( 'com.apple.CrashReporter', 'DialogType', 'none' );
}

1;
