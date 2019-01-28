# pyenv.pl
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;

git_clone( 'https://github.com/pyenv/pyenv.git', "${ENV{HOME}}/.pyenv" );
git_clone(
    'https://github.com/pyenv/pyenv-virtualenv.git',
    "${ENV{HOME}}/.pyenv/plugins/pyenv-virtualenv"
);

1;
