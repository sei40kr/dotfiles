# author: Seong Yong-ju <sei40kr@gmail.com>

# rustup_toolchain_install TOOLCHAIN
#
# Install a Rust toolchain with Rustup.
#
rustup_toolchain_install() {
  assert_command_exists rustup

  local toolchain="$1"

  print-step "Installing Rust ${toolchain} toolchain"

  run_process rustup toolchain install "$toolchain"
}

# rustup_toolchain_install TOOLCHAIN COMPONENT ...
#
# Add Rust components to specified version of Rust with Rustup.
#
rustup_component_add() {
  assert_command_exists rustup

  local toolchain="$1"
  shift
  local components=("$@")

  local stable_toolchain
  if [[ "$toolchain" == stable ]]; then
    stable_toolchain=1
  fi

  for component in "${components[@]}"; do
    print-step "Installing ${component}${stable_toolchain:- to Rust ${toolchain}}"
  done

  run_process rustup component add --toolchain "$toolchain" "${components[@]}"
}
