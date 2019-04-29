# intellij-idea.pl --- IntelliJ IDEA
# author: Seong Yong-ju <sei40kr@gmail.com>

use utf8;
use strict;
use warnings;
use FindBin;
use lib "${FindBin::Bin}/utils/installer/lib";
use Install::PathResolver;

if (&is_macos) {
    brew_cask_install('intellij-idea');

    ln( dotfile('intellij-idea/ideavimrc'), "${ENV{HOME}}/.ideavimrc" );
}

1;
