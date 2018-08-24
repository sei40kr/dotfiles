# utility.bash --- Utility functions for the install script
# author: Seong Yong-ju <sei40kr@gmail.com>

is_arch() {
    [[ -f '/etc/arch-release' ]]
}

is_macos() {
    [[ "$OSTYPE" == darwin* ]]
}

is_linux() {
    [[ "$OSTYPE" == linux* ]]
}
