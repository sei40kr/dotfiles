# macos-tweak-mail.pl --- Mail tweaker
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;

if (&is_macos) {

    # Disable send and reply animations in Mail.app
    defaults_write_bool( 'com.apple.mail', 'DisableReplyAnimations', 1 );
    defaults_write_bool( 'com.apple.mail', 'DisableSendAnimations',  1 );

# Copy email addresses as `foo@example.com` instead of `Foo Bar <foo@example.com>` in Mail.app
    defaults_write_bool( 'com.apple.mail', 'AddressesIncludeNameOnPasteboard',
        0 );
}

1;
