# fish.pl --- fish shell installer
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;
use FindBin;
use lib "${FindBin::Bin}/utils/installer/lib";
use InstallHelper::Path;

# TODO Install direnv on non-macOS envs
if (&is_macos) {
    brew_install('fish');

    brew_install('direnv');
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
    pacman_sync('fish');

    pacman_sync('fzf');
    pacman_sync('tree');
}

if ( &is_macos or &is_arch ) {
    ln( dotfile('fish/bash_profile'), "${ENV{HOME}}/.bash_profile" );
    ln( dotfile('fish/bashrc'),       "${ENV{HOME}}/.bashrc" );

    # Install fish functions
    my @fish_funcs =
      qw(__fzf_edit_dotfile __fzf_ghq __fzf_git_checkout capit cat cd-gitroot diff du fish_greeting fish_user_key_bindings ls magit ping preview ranger-cd ssh top tree);
    ln( dotfile("fish/functions/${_}.fish"),
        "${ENV{XDG_CONFIG_HOME}}/fish/functions/${_}.fish" )
      foreach @fish_funcs;

    # Install fish completions
    my @fish_completions = qw(rustup);
    ln( dotfile("fish/completions/${_}.fish"),
        "${ENV{XDG_CONFIG_HOME}}/fish/completions/${_}.fish" )
      foreach @fish_completions;

    # Install fish config
    my @fish_conf_d = ('alias');
    ln( dotfile("fish/conf.d/${_}.fish"),
        "${ENV{XDG_CONFIG_HOME}}/fish/conf.d/${_}.fish" )
      foreach @fish_conf_d;

    # Install alias definitions
    ln( dotfile('fish/alias_defs/alias_def.fish'),
        "${ENV{XDG_CONFIG_HOME}}/fish/alias_defs/alias_def.fish" );
    my @alias_defs =
      qw( arch docker docker_compose go homebrew java nmap node perl python rsync ruby );
    ln( dotfile("fish/alias_defs/alias_def_${_}.fish"),
        "${ENV{XDG_CONFIG_HOME}}/fish/alias_defs/alias_def_${_}.fish" )
      foreach @alias_defs;

    ln( dotfile('fish/config.fish'),
        "${ENV{XDG_CONFIG_HOME}}/fish/config.fish" );

    # Install Fisherman
    curl( 'https://git.io/fisher',
        "${ENV{XDG_CONFIG_HOME}}/fish/functions/fisher.fish" );

    # Install Fisherman config
    cp( dotfile('fish/fishfile'), "${ENV{XDG_CONFIG_HOME}}/fish/fishfile" );

    # Install fish plugins
    git_clone(
        'git@github.com:sei40kr/fish-ranger-cd.git',
        "${ENV{XDG_CONFIG_HOME}}/fish/plugins/fish-ranger-cd"
    );
    git_clone(
        'git@github.com:sei40kr/fish-tmux-navigator.git',
        "${ENV{XDG_CONFIG_HOME}}/fish/plugins/fish-tmux-navigator"
    );
}

1;
