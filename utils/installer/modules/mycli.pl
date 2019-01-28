# mycli.pl --- mycli installer
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;
use FindBin;
use lib "${FindBin::Bin}/utils/installer/lib";
use InstallHelper::Path;

pip3_install('mycli');
ln( dotfile('mycli/myclirc'), "${ENV{HOME}}/.myclirc" );

1;
