# author: Seong Yong-ju <sei40kr@gmail.com>

install_rust_language_tools() {
  if is_archlinux; then
    pacman_sync rustup
  else
    unsupported_platform_error
  fi

  rustup_toolchain_install stable

  rustup_component_add stable rust-src

  # Build and install rust-analyzer
  if ! command_exists rust-analyzer; then
    assert_command_exists cargo
    git_clone_build rust-analyzer/rust-analyzer \
      'cargo xtask install --server'
  fi
}
