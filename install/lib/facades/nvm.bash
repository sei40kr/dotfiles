# author: Seong Yong-ju <sei40kr@gmail.com>

__verify_nvm() {
  if [[ ! -f "${NVM_DIR}/nvm.sh" ]]; then
    tui-error 'nvm not found. Aborting.'
    exit 1
  fi
}

# nvm_install VERSION
#
# Install specified version of Node.js with nvm.
#
nvm_install() {
  local node_version="$1"

  __verify_nvm

  print-list-item "Installing Node.js ${node_version}"

  (
    . "${NVM_DIR}/nvm.sh"
    run_process nvm install --no-progress "$node_version"
  )
}
