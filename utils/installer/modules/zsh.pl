# zsh.pl --- Z-shell installer
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;
use FindBin;
use lib "${FindBin::Bin}/utils/installer/lib";
use Install::PathResolver;

if (&is_macos) {
    brew_install('zsh');

    brew_install('fzf');
    brew_install('terminal-notifier');

    brew_install('bat');
    brew_install('diff-so-fancy');
    brew_install('exa');
    brew_install('htop');
    brew_install('ncdu');
    brew_install('prettyping');
    brew_install('tree');
} elsif (&is_arch) {
    pacman_sync('zsh');

    pacman_sync('fzf');

    pacman_sync('bat');
    pacman_sync('diff-so-fancy');
    pacman_sync('htop');
    pacman_sync('ncdu');
    pacman_sync('prettyping');
    pacman_sync('tree');

    trizen_sync('exa');
}

git_clone( 'https://github.com/zdharma/zplugin.git',
    "${ENV{HOME}}/.zplugin/bin" );

ln( dotfile('zsh/zshenv'),         "${ENV{HOME}}/.zshenv" );
ln( dotfile('zsh/zshenv'),         "${ENV{HOME}}/.zsh/.zshenv" );
ln( dotfile('zsh/zshrc'),          "${ENV{HOME}}/.zsh/.zshrc" );
ln( dotfile('zsh/alias_defs.zsh'), "${ENV{HOME}}/.zsh/alias_defs.zsh" );
ln( dotfile('zsh/func_defs.zsh'),  "${ENV{HOME}}/.zsh/func_defs.zsh" );

# Install completions
ln(
    dotfile('zsh/completions/_rustup'),
    "${ENV{HOME}}/.zsh/completions/_rustup"
);

# Install functions
my @zsh_funcs = qw(fzf-repo-widget ranger-cd);
ln( dotfile("zsh/functions/${_}"), "${ENV{HOME}}/.zsh/functions/${_}" )
  foreach @zsh_funcs;

1;
