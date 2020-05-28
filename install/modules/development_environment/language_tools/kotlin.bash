# author: Seong Yong-ju <sei40kr@gmail.com>

install_kotlin_language_tools() {
  if is_macos; then
    brew_install kotlin ktlint
    # TODO Install kotlin-language-server
  elif is_archlinux; then
    pacman_sync kotlin
    trizen_sync ktlint kotlin-language-server
  else
    unsupported_platform_error
  fi
}
