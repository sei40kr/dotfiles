# author: Seong Yong-ju <sei40kr@gmail.com>

install_ruby_language_tools() {
  if is_macos; then
    brew_install ruby
  elif is_archlinux; then
    pacman_sync ruby
  else
    unsupported_platform_error
  fi

  git_clone rbenv/rbenv "$RBENV_ROOT"
  mkdir -p "${RBENV_ROOT}/plugins"
  git_clone rbenv/ruby-build "${RBENV_ROOT}/plugins/ruby-build"

  "${RBENV_ROOT}/bin/rbenv" global system

  gem_install system \
    solargraph \
    rubocop \
    rake
}
