# go.pl --- Go installer
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;

goenv_install('1.11.1');
goenv_global('1.11.1');

go_get('github.com/shurcooL/goexec');

1;
