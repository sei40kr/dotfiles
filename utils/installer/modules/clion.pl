# clion.pl --- CLion installer
# author: Seong Yong-ju <sei40kr@gmail.com>

if (&is_macos) {
    brew_cask_install('clion');
}

1;
