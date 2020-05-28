# author: Seong Yong-ju <sei40kr@gmail.com>

install_scala_language_tools() {
  if is_macos; then
    # TODO Install scalafmt
    brew_install \
      scala sbt \
      scalastyle \
      maven gradle
  elif is_archlinux; then
    pacman_sync \
      scala sbt \
      maven gradle
    trizen_sync \
      scalastyle \
      scalafmt-native \
      metals
  else
    unsupported_platform_error
  fi
}
