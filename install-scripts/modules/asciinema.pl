# asciinema.pl
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;

brew_install('imagemagick');
brew_install('giflossy');

pip3_install('asciinema');

yarn_global_add('asciicast2gif');

1;
