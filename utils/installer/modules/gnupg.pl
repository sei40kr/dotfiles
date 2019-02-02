# gnupg.pl --- GnuPG installer
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;
use FindBin;
use lib "${FindBin::Bin}/utils/installer/lib";
use InstallHelper::Path;

if (&is_macos) {
    brew_install('gnupg');
    brew_install('pinentry-mac');

    ln( dotfile('gnupg/gpg-agent.conf'),
        "${ENV{HOME}}/.gnupg/gpg-agent.conf" );
} else {
    # TODO Install GnuPG on other envs
}

1;
