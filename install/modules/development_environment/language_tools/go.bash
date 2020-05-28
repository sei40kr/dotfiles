# author: Seong Yong-ju <sei40kr@gmail.com>

install_go_language_tools() {
  if is_macos; then
    brew_install go
  elif is_archlinux; then
    pacman_sync go
  else
    unsupported_platform_error
  fi

  git_clone syndbg/goenv "$GOENV_ROOT"

  "${GOENV_ROOT}/bin/goenv" global system

  go_get system \
    golang.org/x/tools/gopls@latest \
    golang.org/x/tools/cmd/goimports \
    github.com/motemen/gore/cmd/gore
}
