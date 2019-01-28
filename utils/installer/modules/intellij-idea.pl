# intellij-idea.pl --- IntelliJ IDEA
# author: Seong Yong-ju <sei40kr@gmail.com>

if (&is_macos) {
    brew_cask_install('intellij-idea');
}

1;
