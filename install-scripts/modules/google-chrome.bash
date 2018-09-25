# google-chrome.bash --- Google Chrome installer
# author: Seong Yong-ju <sei40kr@gmail.com>

if is_macos; then
    brew_cask_install_facade google-chrome
elif is_arch; then
    trizen_sync_facade google-chrome
fi
