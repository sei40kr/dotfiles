# macos-tweak-textedit.pl --- Text Edit tweaker
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;

if (&is_macos) {

    # Use plain text mode for new TextEdit documents
    defaults_write_int( 'com.apple.TextEdit', 'RichText', 0 );

    # Open and save files as UTF-8 in TextEdit
    defaults_write_int( 'com.apple.TextEdit', 'PlainTextEncoding',         4 );
    defaults_write_int( 'com.apple.TextEdit', 'PlainTextEncodingForWrite', 4 );
}

1;
