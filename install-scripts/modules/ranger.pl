# ranger.pl --- ranger installer
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;

pip3_install('ranger-fm');

ln( 'ranger/rc.conf',     "${ENV{XDG_CONFIG_HOME}}/ranger/rc.conf" );
ln( 'ranger/commands.py', "${ENV{XDG_CONFIG_HOME}}/ranger/commands.py" );

1;
