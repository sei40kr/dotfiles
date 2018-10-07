# fish.pl --- fish shell installer
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;

if (&is_macos) {
    brew_install('fish');
    brew_install('fzf');

    brew_install('bat');
    brew_install('diff-so-fancy');
    brew_install('htop');
    brew_install('prettyping');
}
elsif (&is_arch) {
    pacman_sync('fish');
    pacman_sync('fzf');

    # TODO Install bat, diff-so-fancy, htop, prettyping
}

if ( &is_macos or &is_arch ) {
    ln( 'fish/bash_profile', "${ENV{HOME}}/.bash_profile" );
    ln( 'fish/bashrc',       "${ENV{HOME}}/.bashrc" );

    my @fish_funcs = qw(balias cat diff ping preview top);
    ln( "fish/functions/${_}.fish",
        "${ENV{XDG_CONFIG_HOME}}/fish/functions/${_}.fish" )
      foreach @fish_funcs;

    ln( 'fish/conf.d/balias_def.fish',
        "${ENV{XDG_CONFIG_HOME}}/fish/conf.d/balias_def.fish" );
    my @balias_defs =
      qw( arch docker_compose go homebrew java nmap node perl python rsync ruby );
    ln( "fish/conf.d/balias_def_${_}.fish",
        "${ENV{XDG_CONFIG_HOME}}/fish/conf.d/balias_def_${_}.fish" )
      foreach @balias_defs;

    ln( 'fish/config.fish', "${ENV{XDG_CONFIG_HOME}}/fish/config.fish" );

    curl( 'https://git.io/fisher',
        "${ENV{XDG_CONFIG_HOME}}/fish/functions/fisher.fish" );
}

1;
