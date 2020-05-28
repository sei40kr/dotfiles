# author: Seong Yong-ju <sei40kr@gmail.com>

install_tools() {
  while true; do
    print_title 'Tools'

    tui_add_options \
      'Git (+ git-crypt)' install_git \
      'EditorConfig' install_editorconfig
    if is_macos; then
      tui_add_options 'Dash' install_dash
    elif is_archlinux; then
      tui_add_options 'Zeal' install_zeal
    fi
    tui_set_quit_option d 'Done'

    if ! tui_select_options 'Enter your option'; then
      break
    fi
  done
}

install_git() {
  if is_macos; then
    brew_install git git-crypt
  elif is_archlinux; then
    pacman_sync git git-crypt
  else
    unsupported_platform_error
  fi

  mkdir -p "${XDG_CONFIG_HOME}/git"
  ln -fs "${HOME}/.dotfiles/git/config" "${XDG_CONFIG_HOME}/git/config"
  ln -fs "${HOME}/.dotfiles/git/ignore" "${XDG_CONFIG_HOME}/git/ignore"
}

install_editorconfig() {
  if is_macos; then
    brew_install editorconfig
  elif is_arch; then
    pacman_sync editorconfig-core-c
  else
    unsupported_platform_error
  fi
}

install_dash() {
  assert_macos
  brew_cask_install dash
}

install_zeal() {
  assert_archlinux
  pacman_sync zeal
}
