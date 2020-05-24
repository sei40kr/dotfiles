# author: Seong Yong-ju <sei40kr@gmail.com>

# rbenv_install VERSION
#
# Install specified version of Ruby with rbenv.
#
rbenv_install() {
  assert_executable "${RBENV_ROOT}/bin/rbenv"

  local ruby_version="$1"

  print-step "Installing Ruby v${ruby_version}"

  run_process "${RBENV_ROOT}/bin/rbenv" install -s "$ruby_version"
}
