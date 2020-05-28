# author: Seong Yong-ju <sei40kr@gmail.com>

install_shell_script_language_tools() {
  if is_macos; then
    brew_install shellcheck shfmt
  elif is_archlinux; then
    pacman_sync shellcheck shfmt
  else
    unsupported_platform_error
  fi

  yarn_global_add system bash-language-server
}
