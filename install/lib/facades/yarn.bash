# author: Seong Yong-ju <sei40kr@gmail.com>

# yarn_global_add VERSION PACKAGE ...
#
# Install Yarn packages to specified version of Node.js.
#
yarn_global_add() {
  assert_command_exists yarn

  local node_version="$1"
  shift
  local -a packages=("$@")

  local system_node
  local node_exec_dir
  if [[ "$node_version" == system ]]; then
    system_node=1

    if is_macos; then
      node_exec_dir=/usr/local/bin/node
    else
      node_exec_dir=/usr/bin/node
    fi

    assert_executable "${node_exec_dir}/node" \
      'node executable of system-installed version not found. Aborting.'
  else
    node_exec_dir="${NVM_DIR}/versions/${node_version}/bin"

    assert_executable "$node_exec_dir" \
      "node executable of ${node_version} not found. Aborting."
  fi

  for package in "${packages[@]}"; do
    print-list-item "Installing ${package}${system_node:- to Node.js ${node_version}}"
  done

  run_process PATH="${node_exec_dir}:${PATH}" yarn global add --no-default-rc --noprogress --non-interactive "${packages[@]}"
}
