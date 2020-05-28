# author: Seong Yong-ju <sei40kr@gmail.com>

install_java_language_tools() {
  if is_macos; then
    brew_install 'openjdk@11' maven gradle
  elif is_archlinux; then
    pacman_sync jdk11-openjdk maven gradle
  else
    unsupported_platform_error
  fi

  git_clone jenv/jenv "$JENV_ROOT"
  # Make sure $JAVA_HOME is set
  local PATH="${JENV_ROOT}/bin:${PATH}"
  jenv enable-plugin export
}
