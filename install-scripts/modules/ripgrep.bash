# ripgrep.bash --- ripgrep installer
# author: Seong Yong-ju <sei40kr@gmail.com>

if is_arch; then
    pacman_sync_facade ripgrep

    ln_facade "${DOTFILES_PATH}/ripgrep/ripgreprc" "${HOME}/.ripgreprc"
fi
