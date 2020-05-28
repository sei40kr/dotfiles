# author: Seong Yong-ju <sei40kr@gmail.com>

install_system_tools() {
  while true; do
    print_title 'System Tools'

    tui_add_options \
      'htop' install_htop \
      'tcpdump' install_tcpdump
    if is_archlinux; then
      tui_add_options 'strace' install_strace
    fi
    tui_set_quit_option d 'Done'

    if ! tui_select_options 'Enter your option'; then
      break
    fi
  done
}

install_htop() {
  if is_macos; then
    brew_install htop
  elif is_archlinux; then
    pacman_sync htop
  else
    unsupported_platform_error
  fi
}

install_tcpdump() {
  if is_macos; then
    brew_install tcpdump
  elif is_archlinux; then
    pacman_sync tcpdump
  else
    unsupported_platform_error
  fi
}

install_strace() {
  assert_archlinux
  pacman_sync strace
}
