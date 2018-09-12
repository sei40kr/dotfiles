# git.bash --- Git installer
# author: Seong Yong-ju <sei40kr@gmail.com>

if is_arch; then
    pacman_sync_facade git

    ln_facade "${DOTFILES_PATH}/git/gitconfig" "${HOME}/.gitconfig"
fi
