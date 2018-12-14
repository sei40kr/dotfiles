# ruby.pl --- Ruby installer
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;

rbenv_install('2.5.1');
rbenv_global('2.5.1');

1;
