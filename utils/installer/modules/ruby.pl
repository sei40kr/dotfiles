# ruby.pl --- Ruby installer
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;

# install rbenv
git_clone( 'https://github.com/rbenv/rbenv.git', "${ENV{HOME}}/.rbenv" );

# install ruby-build
git_clone(
    'https://github.com/rbenv/ruby-build.git',
    "${ENV{HOME}}/.rbenv/plugins/ruby-build"
);

rbenv_install('2.5.1');
rbenv_global('2.5.1');

1;
