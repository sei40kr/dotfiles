# nvm.pl --- nvm installer
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;

git_clone( 'https://github.com/creationix/nvm.git', "${ENV{HOME}}/.nvm" );

1;
