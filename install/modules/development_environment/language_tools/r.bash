# author: Seong Yong-ju <sei40kr@gmail.com>

install_r_language_tools() {
  if is_macos; then
    brew_install r
  elif is_archlinux; then
    pacman_sync r tk openblas
  else
    unsupported_platform_error
  fi

  ln -fs "${HOME}/.dotfiles/r/Renviron" "${HOME}/.Renviron"
  ln -fs "${HOME}/.dotfiles/r/Renviron" "$R_ENVIRON_USER"
  ln -fs "${HOME}/.dotfiles/r/Rprofile.r" "$R_PROFILE_USER"

  r_install lintr languageserver
}
