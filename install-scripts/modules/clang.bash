# clang.bash --- Clang installer
# author: Seong Yong-ju <sei40kr@gmail.com>

if is_arch; then
    pacman_sync_facade clang

    ln_facade "${DOTFILES_PATH}/clang-format/clang-format" "${HOME}/.clang-format"
fi
