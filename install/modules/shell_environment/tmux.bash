# author: Seong Yong-ju <sei40kr@gmail.com>

install_tmux() {
  if is_macos; then
    brew_install tmux fzf
  elif is_archlinux; then
    pacman_sync tmux xsel fzf
  else
    unsupported_platform_error
  fi

  ln -fs "${HOME}/.dotfiles/tmux/tmux.conf" "${HOME}/.tmux.conf"

  mkdir -p "${HOME}/.tmux/scripts"
  ln -fs "${HOME}/.dotfiles/tmux/scripts/clean-orphan-sessions.bash" \
    "${HOME}/.tmux/scripts/clean-orphan-sessions.bash"

  mkdir -p "${HOME}/.tmux/plugins"
  git_clone tmux-plugins/tpm "${HOME}/.tmux/plugins/tpm"
  "${HOME}/.tmux/plugins/tpm/bin/install_plugins"
}
