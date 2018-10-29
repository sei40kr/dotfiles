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
    brew_install('exa');
    brew_install('htop');
    brew_install('ncdu');
    brew_install('prettyping');
    brew_install('tree');
}
elsif (&is_arch) {

    # TODO Install tree
    pacman_sync('fish');
    pacman_sync('fzf');
}

if ( &is_macos or &is_arch ) {
    ln( 'fish/bash_profile', "${ENV{HOME}}/.bash_profile" );
    ln( 'fish/bashrc',       "${ENV{HOME}}/.bashrc" );

    # Install fish functions
    my @fish_funcs =
      qw(__fzf_edit_dotfile __fzf_ghq __fzf_git_checkout balias capit cat diff du fish_greeting fish_title fish_user_key_bindings ls ping preview ssh top tree);
    ln( "fish/functions/${_}.fish",
        "${ENV{XDG_CONFIG_HOME}}/fish/functions/${_}.fish" )
      foreach @fish_funcs;

    # Install fish completions
    my @fish_completions = qw(rustup);
    ln( "fish/completions/${_}.fish",
        "${ENV{XDG_CONFIG_HOME}}/fish/completions/${_}.fish" )
      foreach @fish_completions;

    # Install fish config
    my @fish_conf_d = qw(fzf tmux);
    ln( "fish/conf.d/${_}.fish",
        "${ENV{XDG_CONFIG_HOME}}/fish/conf.d/${_}.fish" )
      foreach @fish_conf_d;

    # Install balias definitions
    ln( 'fish/conf.d/balias_def.fish',
        "${ENV{XDG_CONFIG_HOME}}/fish/conf.d/balias_def.fish" );
    my @balias_defs =
      qw( arch docker docker_compose go homebrew java nmap node perl python rsync ruby );
    ln( "fish/conf.d/balias_def_${_}.fish",
        "${ENV{XDG_CONFIG_HOME}}/fish/conf.d/balias_def_${_}.fish" )
      foreach @balias_defs;

    ln( 'fish/config.fish', "${ENV{XDG_CONFIG_HOME}}/fish/config.fish" );

    # Install Fisherman
    curl( 'https://git.io/fisher',
        "${ENV{XDG_CONFIG_HOME}}/fish/functions/fisher.fish" );

    # Install Fisherman config
    ln( 'fish/fishfile', "${ENV{XDG_CONFIG_HOME}}/fish/fishfile" );
}

1;
