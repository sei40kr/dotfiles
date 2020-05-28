# author: Seong Yong-ju <sei40kr@gmail.com>

install_groovy_language_tools() {
  if is_macos; then
    brew_install groovy
    # TODO Install groovy-language-server
  elif is_archlinux; then
    pacman_sync groovy
    trizen_sync groovy-language-server-git
  else
    unsupported_platform_error
  fi
}
