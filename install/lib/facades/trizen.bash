# author: Seong Yong-ju <sei40kr@gmail.com>

# trizen_sync PACKAGE ...
#
# Install packages with Trizen. Works only on Arch Linux.
#
trizen_sync() {
  assert_archlinux
  assert_command_exists trizen

  local -a packages=("$@")

  for package in "${packages[@]}"; do
    print-list-item "Installing ${package}"
  done

  run_process trizen -Sy --needed --noconfirm --noprogressbar --nopull "${packages[@]}"
}
