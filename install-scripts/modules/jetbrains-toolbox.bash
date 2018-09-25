# jetbrains-toolbox.bash --- JetBrains Toolbox
# author: Seong Yong-ju <sei40kr@gmail.com>

if is_macos; then
    brew_cask_install_facade jetbrains-toolbox
elif is_arch; then
    trizen_sync_facade jetbrains-toolbox
fi
