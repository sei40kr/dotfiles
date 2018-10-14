# xmonad.pl --- XMonad installer
# author: Seong Yong-ju <sei40kr@gmail.com>

if (&is_arch) {
    stack_install('xmonad');
    stack_install('xmonad-contribs');

    stack_install('xmobar', 'with_xft', 'with_utf8');
}

1;
