# findutils.pl --- findutils installer
# author: Seong Yong-ju <sei40kr@gmail.com>

if (&is_macos) {
    brew_install('findutils');
}

1;
