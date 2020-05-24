# author: Seong Yong-ju <sei40kr@gmail.com>

# goenv_install VERSION
#
# Install specified version of Go with goenv.
#
goenv_install() {
  assert_executable "${GOENV_ROOT}/bin/goenv"

  local go_version="$1"

  print-step "Installing Go v${go_version}"

  run_process "${GOENV_ROOT}/bin/goenv" install -s "$go_version"
}
