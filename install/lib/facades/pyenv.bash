# author: Seong Yong-ju <sei40kr@gmail.com>

# pyenv_install VERSION
#
# Install specified version of Python with pyenv.
#
pyenv_install() {
  assert_executable "${PYENV_ROOT}/bin/pyenv"

  local python_version="$1"

  print-step "Installing Python v${python_version}"

  run_process "${PYENV_ROOT}/bin/pyenv" install -s "$python_version"
}
