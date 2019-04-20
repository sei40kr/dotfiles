# python.pl
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;

# install pyenv
git_clone( 'https://github.com/pyenv/pyenv.git', "${ENV{HOME}}/.pyenv" );

# install pyenv-virtualenv
git_clone(
    'https://github.com/pyenv/pyenv-virtualenv.git',
    "${ENV{HOME}}/.pyenv/plugins/pyenv-virtualenv"
);

pyenv_install('3.6.6');
pyenv_install('2.7.15');
pyenv_global( '3.6.6', '2.7.15' );

# install poetry
pip3_install('poetry');

1;
