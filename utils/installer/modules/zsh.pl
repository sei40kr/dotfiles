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

ln( dotfile('zsh/zshenv'),         "${ENV{HOME}}/.zshenv" );
ln( dotfile('zsh/zshenv'),         "${ENV{HOME}}/.zsh/.zshenv" );
ln( dotfile('zsh/zshrc'),          "${ENV{HOME}}/.zsh/.zshrc" );
ln( dotfile('zsh/alias_defs.zsh'), "${ENV{HOME}}/.zsh/alias_defs.zsh" );

1;
