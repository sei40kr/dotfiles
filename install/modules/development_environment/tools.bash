# author: Seong Yong-ju <sei40kr@gmail.com>

install_tools() {
  while true; do
    print_title 'Tools'

    tui_add_options \
      'Git' install_tools__git \
      'EditorConfig' install_tools__editorconfig
    if is_macos; then
      tui_add_options 'Dash' install_tools__dash
    elif is_archlinux; then
      tui_add_options 'Zeal' install_tools__zeal
    fi
    tui_set_quit_option d 'Done'

    if ! tui_select_option 'Enter your option'; then
      break
    fi
  done
}

install_tools__git() {
  if is_macos; then
    brew_install git
  elif is_archlinux; then
    pacman_sync git
  else
    unsupported_platform_error
  fi

  mkdir -p "${XDG_CONFIG_HOME}/git"
  ln -fs "${HOME}/.dotfiles/git/config" "${XDG_CONFIG_HOME}/git/config"
  ln -fs "${HOME}/.dotfiles/git/ignore" "${XDG_CONFIG_HOME}/git/ignore"
}

install_tools__editorconfig() {
  if is_macos; then
    brew_install editorconfig
  elif is_arch; then
    pacman_sync editorconfig-core-c
  else
    unsupported_platform_error
  fi
}

install_tools__dash() {
  assert_macos
  brew_cask_install dash
}

install_tools__zeal() {
  assert_archlinux
  pacman_sync zeal
}
