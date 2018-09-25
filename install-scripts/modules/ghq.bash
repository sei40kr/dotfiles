# ghq.bash --- ghq installer
# author: Seong Yong-ju <sei40kr@gmail.com>

if is_macos; then
    brew_install_facade ghq
elif is_arch; then
    trizen_sync_facade ghq
fi
