# node.pl --- Node installer
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;

nvm_install('v10.14.2');
nvm_alias( 'default', 'v10.14.2' );

1;
