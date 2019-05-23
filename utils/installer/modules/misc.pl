# misc.pl
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;

if (&is_macos) {
    brew_install('coreutils');
    brew_install('curl');
    brew_install('ed', 'with-default-names');
    brew_install('fd');
    brew_install('findutils');
    brew_install('gawk');
    brew_install('gnu-sed');
    brew_install('gnu-tar');
    brew_install('grep');
    brew_install('gzip');
    brew_install('jq');
    brew_install('nano');
    brew_install('parallel');
    brew_install('unzip');
    brew_install('wget');
    brew_install('vim');
    brew_install('xmlstarlet');

    brew_tap('tavianator/tap');
    brew_install('bfs');

    brew_cask_install('appcleaner');
    brew_cask_install('discord');
    brew_cask_install('dropbox');
    brew_cask_install('gitter');
    brew_cask_install('plex-media-server');
}
elsif (&is_arch) {
    pacman_sync('fd');
    pacman_sync('jq');

    trizen_sync('bfs');
}

gem_install('reveal-ck');

pip3_install('aws-shell');
pip3_install('pgcli');

yarn_global_add('@storybook/cli');
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

1;
