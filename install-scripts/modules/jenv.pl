# jenv.pl --- jenv installer
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;

git_clone( 'https://github.com/gcuisinier/jenv.git', "${ENV{HOME}}/.jenv" );

ln( "${ENV{HOME}}/.jenv/fish/export.fish",
    "${ENV{XDG_CONFIG_HOME}}/fish/functions/export.fish" );
ln( "${ENV{HOME}}/.jenv/fish/jenv.fish",
    "${ENV{XDG_CONFIG_HOME}}/fish/functions/jenv.fish" );

1;
