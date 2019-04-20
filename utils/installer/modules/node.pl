# node.pl --- Node.js installer
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;

# install nvm
git_clone( 'https://github.com/creationix/nvm.git', "${ENV{HOME}}/.nvm" );

nvm_install('v10.14.2');
nvm_alias( 'default', 'v10.14.2' );

1;
