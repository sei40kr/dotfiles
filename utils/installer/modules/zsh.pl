# zsh.pl --- Z-shell installer
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;
use FindBin;
use lib "${FindBin::Bin}/utils/installer/lib";
use InstallHelper::Path;

if (&is_macos) {
    brew_install('fzf');
    brew_install('terminal-notifier');
    brew_install('zsh');
}
else {
    # TODO Install zsh on other envs
}

git_clone( 'https://github.com/zdharma/zplugin.git',
    "${ENV{HOME}}/.zplugin/bin" );

ln( dotfile('zsh/zsh_profile'), "${ENV{HOME}}/.zsh_profile" );
ln( dotfile('zsh/zshrc'),       "${ENV{HOME}}/.zshrc" );

1;
