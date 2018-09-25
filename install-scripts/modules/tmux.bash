# tmux.bash --- TMUX installer
# author: Seong Yong-ju <sei40kr@gmail.com>

if is_macos; then
    # TODO Install with --with-utf8proc option
    brew_install_facade tmux
    brew_install_facade reattach-to-user-namespace
elif is_arch; then
    pacman_sync_facade tmux
fi

if is_macos || is_arch; then
    ln_facade "${DOTFILES_PATH}/tmux/tmux.conf" "${HOME}/.tmux.conf"
    ln_facade "${DOTFILES_PATH}/tmux/tmux-cpu-usage" "${HOME}/.local/bin/tmux-cpu-usage"
    ln_facade "${DOTFILES_PATH}/tmux/tmux-mem-usage" "${HOME}/.local/bin/tmux-mem-usage"
fi
