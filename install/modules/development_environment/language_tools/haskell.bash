# author: Seong Yong-ju <sei40kr@gmail.com>

install_haskell_language_tools() {
  if is_macos; then
    brew_install haskell-stack
  elif is_archlinux; then
    pacman_sync stack
  else
    unsupported_platform_error
  fi

  mkdir -p "${HOME}/.stack"
  ln -fs "${HOME}/.dotfiles/stack/config.yaml" "${HOME}/.stack/config.yaml"

  stack_install hlint brittany

  # Build and install haskell-ide-engine
  if ! command_exists hie-wrapper; then
    assert_command_exists stack
    git_clone_build haskell/haskell-ide-engine 'stack ./install.hs hie'
  fi
}
