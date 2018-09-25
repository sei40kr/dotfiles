# git.bash --- Git installer
# author: Seong Yong-ju <sei40kr@gmail.com>

if is_macos; then
    brew_install_facade git
elif is_arch; then
    pacman_sync_facade git
fi

if is_macos || is_arch; then
    ln_facade "${DOTFILES_PATH}/git/gitconfig" "${HOME}/.gitconfig"
fi
