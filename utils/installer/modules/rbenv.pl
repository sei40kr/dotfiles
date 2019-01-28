# rbenv.pl --- rbenv installer
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;

git_clone( 'https://github.com/rbenv/rbenv.git', "${ENV{HOME}}/.rbenv" );
git_clone(
    'https://github.com/rbenv/ruby-build.git',
    "${ENV{HOME}}/.rbenv/plugins/ruby-build"
);

1;
