# ripgrep.bash --- ripgrep installer
# author: Seong Yong-ju <sei40kr@gmail.com>

if is_macos; then
    brew_tap_facade burntsushi/ripgrep https://github.com/BurntSushi/ripgrep.git
    brew_install_facade ripgrep-bin
elif is_arch; then
    pacman_sync_facade ripgrep
fi

if is_macos || is_arch; then
    ln_facade "${DOTFILES_PATH}/ripgrep/ripgreprc" "${HOME}/.ripgreprc"
fi
