# goenv.pl --- goenv installer
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;

git_clone( 'https://github.com/syndbg/goenv.git', "${ENV{HOME}}/.goenv" );

1;
