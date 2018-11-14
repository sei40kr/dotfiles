# pyenv.pl
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;

git_clone( 'https://github.com/pyenv/pyenv.git', "${ENV{PYENV_ROOT}}" );
git_clone(
    'https://github.com/pyenv/pyenv-virtualenv.git',
    "${ENV{PYENV_ROOT}}/plugins/pyenv-virtualenv"
);

1;
