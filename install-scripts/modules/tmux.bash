# tmux.bash --- TMUX installer
# author: Seong Yong-ju <sei40kr@gmail.com>

pacman_sync_facade tmux

ln_facade "${DOTFILES_PATH}/tmux/tmux.conf" "${HOME}/.tmux.conf"
ln_facade "${DOTFILES_PATH}/tmux/tmux-cpu-usage" "${HOME}/.local/bin/tmux-cpu-usage"
ln_facade "${DOTFILES_PATH}/tmux/tmux-mem-usage" "${HOME}/.local/bin/tmux-mem-usage"
