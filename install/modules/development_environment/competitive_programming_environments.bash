# author: Seong Yong-ju <sei40kr@gmail.com>

install_competitive_programming_environments() {
  while true; do
    print_title 'Competitive Programming Environments'

    tui_add_options \
      'AtCoder (requires Rust, Python)' install_atcoder \
      'LeetCode (requires Rust, Python)' install_leetcode
    tui_set_quit_option d 'Done'

    if ! tui_select_option 'Enter your option'; then
      break
    fi
  done
}

install_atcoder() {
  rustup_toolchain_install 1.42.0
  rustup_component_add 1.42.0 rust-src

  pyenv_install 3.4.3

  pip_install system atcoder-tools online-judge-tools

  ln -fs "${HOME}/.dotfiles/atcoder-tools/atcodertools.toml" \
    "${HOME}/.atcodertools.toml"
  ln -fs "${HOME}/.dotfiles/atcoder-tools/custom_code_generator.py" \
    "${HOME}/custom_code_generator.py"
  ln -fs "${HOME}/.dotfiles/atcoder-tools/my_template.rs" \
    "${HOME}/my_template.rs"
}

install_leetcode() {
  rustup_toolchain_install 1.31.0
  rustup_component_add 1.31.0 rust-src rls-preview rust-analysis
}
