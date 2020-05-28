# author: Seong Yong-ju <sei40kr@gmail.com>

install_python_language_tools() {
  if is_macos; then
    brew_install python poetry
  elif is_archlinux; then
    pacman_sync python python-pip poetry
  else
    unsupported_platform_error
  fi

  git_clone pyenv/pyenv "$PYENV_ROOT"
  mkdir -p "${PYENV_ROOT}/plugins"
  git_clone pyenv/pyenv-virtualenv "${PYENV_ROOT}/plugins/pyenv-virtualenv"

  "${PYENV_ROOT}/bin/pyenv" global system

  pip_install system \
    'python-language-server[all]' \
    black \
    jupyter numpy pandas matplotlib
}
