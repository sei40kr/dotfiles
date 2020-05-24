# author: Seong Yong-ju <sei40kr@gmail.com>

# stack_install PACKAGE ...
#
# Build and install Haskell packages with Haskell Tool Stack.
#
stack_install() {
  assert_command_exists stack

  local -a packages=("$@")

  for package in "${packages[@]}"; do
    print-list-item "Installing ${package}"
  done

  run_process stack install "${packages[@]}"
}
