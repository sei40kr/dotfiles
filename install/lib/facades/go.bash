# author: Seong Yong-ju <sei40kr@gmail.com>

__go_exec() {
  local version="$1"

  if [[ "$version" == system ]]; then
    if is_macos; then
      echo /usr/local/bin/go
    else
      echo /usr/bin/go
    fi
  else
    echo "${GOENV_ROOT}/versions/${version}/go"
  fi
}

# go_get VERSION PACKAGE ...
#
# Build and install Go packages to specified version of Go.
#
go_get() {
  local go_version="$1"
  shift
  local packages=("$@")

  local go_exec="$(__go_exec "$go_version")"

  if [[ ! -x "$go_exec" ]]; then
    if [[ "$go_version" == system ]]; then
      tui-error "go executable of ${go_version} not found. Aborting."
    else
      tui-error 'go executable of system-installed version not found. Aborting.'
    fi

    exit 1
  fi

  local system_go
  if [[ "$go_version" == system ]]; then
    system_go=1
  fi

  for package in "${packages[@]}"; do
    local short_name
    short_name="${package##*/}"
    short_name="${package%%@*}"
    print-list-item "Installing ${short_name}${system_go:- to Go ${go_version}}"
  done

  run_process GOPATH="${HOME}/go/${go_version}" "$go_exec" get -u "${packages[@]}"
}
