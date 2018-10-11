# fish.pl --- fish shell installer
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;

if (&is_macos) {
    brew_install('fish');
    brew_install('fzf');
    brew_install('terminal-notifier');

    brew_install('bat');
    brew_install('diff-so-fancy');
    brew_install('htop');
    brew_install('ncdu');
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

    # Install fish functions
    my @fish_funcs =
      qw(__fzf_edit_dotfile __fzf_git_checkout balias capit cat diff du fish_greeting fish_user_key_bindings ping preview ssh top);
    ln( "fish/functions/${_}.fish",
        "${ENV{XDG_CONFIG_HOME}}/fish/functions/${_}.fish" )
      foreach @fish_funcs;

    # Install fish completions
    my @fish_completions = qw(rustup);
    ln( "fish/completions/${_}.fish",
        "${ENV{XDG_CONFIG_HOME}}/fish/completions/${_}.fish" )
      foreach @fish_completions;

    # Install balias definitions
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
