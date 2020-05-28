# author: Seong Yong-ju <sei40kr@gmail.com>

install_perl_language_tools() {
  if is_macos; then
    brew_install perl cpanminus
  elif is_archlinux; then
    pacman_sync perl cpanminus
  else
    unsupported_platform_error
  fi

  if [[ ! -d "$PERLBREW_ROOT" ]]; then
    pacman_sync perlbrew
    /usr/bin/vendor_perl/perlbrew init 1>/dev/null
  fi

  # TODO install some Perl modules
}
