# author: Seong Yong-ju <sei40kr@gmail.com>

install_plantuml_language_tools() {
  if is_macos; then
    brew_install plantuml
  elif is_archlinux; then
    trizen_sync plantuml
  else
    unsupported_platform_error
  fi
}
