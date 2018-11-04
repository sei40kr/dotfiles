# printing-prefs.pl
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;

if (&is_macos) {

    # Automatically quit printer app once the print jobs complete
    defaults_write_bool( 'com.apple.print.PrintingPrefs',
        'Quit When Finished', 1 );
}

1;
