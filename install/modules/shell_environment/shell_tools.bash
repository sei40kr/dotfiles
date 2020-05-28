# author: Seong Yong-ju <sei40kr@gmail.com>

install_shell_tools() {
  while true; do
    print_title 'Shell Tools'

    tui_add_options \
      'exa' install_exa \
      'bat' install_bat \
      'fd' install_fd \
      'ripgrep' install_ripgrep \
      'Ranger (requires Python)' install_ranger \
      'asciinema (requires Python)' install_asciinema
    tui_set_quit_option d 'Done'

    if ! tui_select_options 'Enter your option'; then
      break
    fi
  done
}

install_exa() {
  if is_macos; then
    brew_install exa
  elif is_archlinux; then
    pacman_sync exa
  else
    unsupported_platform_error
  fi
}

install_bat() {
  if is_macos; then
    brew_install bat
  elif is_archlinux; then
    pacman_sync bat
  else
    unsupported_platform_error
  fi
}

install_fd() {
  if is_macos; then
    brew_install fd
  elif is_archlinux; then
    pacman_sync fd
  else
    unsupported_platform_error
  fi
}

install_ripgrep() {
  if is_macos; then
    brew_install ripgrep
  elif is_archlinux; then
    pacman_sync ripgrep
  else
    unsupported_platform_error
  fi

  ln -fs "${HOME}/.dotfiles/ripgrep/ripgreprc" "${HOME}/.ripgreprc"
}

install_ranger() {
  pip_install system ranger-fm

  ln -fs "${HOME}/.dotfiles/ranger/rc.conf" "${XDG_CONFIG_HOME}/ranger/rc.conf"
  ln -fs "${HOME}/.dotfiles/ranger/commands.py" "${XDG_CONFIG_HOME}/ranger/commands.py"
}

install_asciinema() {
  pip_install system asciinema
}
