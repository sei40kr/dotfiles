# coreutils.pl --- coreutils installer
# author: Seong Yong-ju <sei40kr@gmail.com>

if (&is_macos) {
    brew_install('coreutils');
}

1;
