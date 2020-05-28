# author: Seong Yong-ju <sei40kr@gmail.com>

install_c_language_tools() {
  if is_macos; then
    brew_install llvm gcc gdb clang-format ccls
  elif is_archlinux; then
    pacman_sync llvm gcc gdb gdb-dashboard clang
    trizen_sync ccls
  else
    unsupported_platform_error
  fi

  pip_install system cpplint cmakelint

  ln -fs "/usr/share/gdb-dashboard/.gdbinit" "${HOME}/.gdbinit"
}
