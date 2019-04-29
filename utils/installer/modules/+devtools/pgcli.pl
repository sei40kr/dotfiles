# pgcli.pl --- pgcli installer
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;
use FindBin;
use lib "${FindBin::Bin}/utils/installer/lib";
use Install::PathResolver;

pip3_install('pgcli');
ln( dotfile('pgcli/pgclirc'), "${ENV{HOME}}/.config/pgcli/config" );

1;
