# phpbrew.pl --- phpbrew installer
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;

curl( 'https://github.com/phpbrew/phpbrew/raw/master/phpbrew',
    "${ENV{HOME}}/.local/bin/phpbrew" );
# TODO Move phpbrew to /usr/local/bin
chmod_facade( 755, "${ENV{HOME}}/.local/bin/phpbrew" );

1;
