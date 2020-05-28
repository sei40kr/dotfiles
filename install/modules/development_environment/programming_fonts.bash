# author: Seong Yong-ju <sei40kr@gmail.com>

install_programming_fonts() {
  while true; do
    print_title 'Programming Fonts'

    tui_init_options
    tui_add_options \
      'Fira Code' install_fira_code \
      'Source Code Pro' install_source_code_pro \
      'JetBrains Mono' install_jetbrains_mono
    tui_set_quit_option d 'Done'

    if ! tui_select_option 'Enter your option'; then
      break
    fi
  done
}

install_fira_code() {
  if is_macos; then
    brew_cask_install font-fira-code
  elif is_archlinux; then
    pacman_sync otf-fira-code
  else
    unsupported_platform_error
  fi
}

install_source_code_pro() {
  if is_macos; then
    brew_cask_install font-source-code-pro
  elif is_archlinux; then
    pacman_sync adobe-source-code-pro-fonts
  else
    unsupported_platform_error
  fi
}

install_jetbrains_mono() {
  if is_macos; then
    brew_cask_install font-jetbrains-mono
  elif is_archlinux; then
    pacman_sync ttf-jetbrains-mono
  else
    unsupported_platform_error
  fi
}
