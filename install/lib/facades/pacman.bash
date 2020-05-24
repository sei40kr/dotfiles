# author: Seong Yong-ju <sei40kr@gmail.com>

# pacman_sync PACKAGE ...
#
# Install packages with Pacman. Works only on Arch Linux.
#
pacman_sync() {
  assert_archlinux
  assert_command_exists pacman

  local -a packages=("$@")

  for package in "${packages[@]}"; do
    print-list-item "Installing ${package}"
  done

  run_process sudo pacman -Sy --needed --noconfirm --noprogressbar "${packages[@]}"
}
