# author: Seong Yong-ju <sei40kr@gmail.com>

install_latex_language_tools() {
  if is_macos; then
    brew_cask_install basictex
  elif is_archlinux; then
    pacman_sync texlive-most texlive-langjapanese
  else
    unsupported_platform_error
  fi
}
