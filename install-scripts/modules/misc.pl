# misc.pl
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;

if (&is_macos) {
    brew_install('fd');
    brew_install('jq');

    brew_tap('tavianator/tap');
    brew_install('bfs');

    brew_cask_install('plex-media-server');
}
elsif (&is_arch) {
    pacman_sync('fd');
    pacman_sync('jq');

    trizen_sync('bfs');

    pip3_install('asciinema');
    pip3_install('aws-shell');

    yarn_global_add('create-react-app');
    yarn_global_add('create-react-native-app');
    yarn_global_add('flow-bin');
    yarn_global_add('gatsby-cli');
    yarn_global_add('generate');
    yarn_global_add('generate-editorconfig');
    yarn_global_add('generate-gitignore');
    yarn_global_add('generate-license');
    yarn_global_add('generate-project');
    yarn_global_add('gulp');
    yarn_global_add('ngrok');
    yarn_global_add('typescript');
    yarn_global_add('webpack');
    yarn_global_add('yo');
}

1;
