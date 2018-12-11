# python.pl
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;

pyenv_install('3.6.6');
pyenv_install('2.7.15');
pyenv_global( '3.6.6', '2.7.15' );

1;
