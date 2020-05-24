# author: Seong Yong-ju <sei40kr@gmail.com>

# pip_install VERSION PACKAGE ...
#
# Install Python packages to specified version of Python.
#
pip_install() {
  local python_version="$1"
  shift
  local -a packages=("$@")
  local -a pip_opts=(-q -U)

  local pip_exec
  if [[ "$python_version" == system ]]; then
    if is_macos; then
      pip_exec=/usr/local/bin/pip
    else
      pip_exec=/usr/bin/pip
    fi

    assert_executable "$pip_exec" \
      'pip executable of system-installed version not found. Aborting.'

    pip_opts+=(--user)
  else
    pip_exec="${PYENV_ROOT}/versions/${python_version}/bin/pip"

    assert_executable "$pip_exec" \
      "pip executable of ${python_version} not found. Aborting."
  fi

  for package in "${packages[@]}"; do
    print-list-item "Installing ${package}"
  done

  run_process "$pip_exec" --disable-pip-version-check install "${pip_opts[@]}" "${packages[@]}"
}
