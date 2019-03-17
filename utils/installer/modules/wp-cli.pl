# wp-cli.pl --- wp-cli installer
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;

if (&is_macos) {
    brew_install('wp-cli');
    brew_install('wp-cli-completion');
}

# TODO install wp-cli on other envs

1;
