# author: Seong Yong-ju <sei40kr@gmail.com>

install_infrastructure_cicd_tools() {
  while true; do
    print_title 'Infrastructure & CI/CD Tools'

    tui_init_options
    tui_add_options \
      'AWS Shell (requires Python)' install_aws_shell \
      'Google Cloud SDK' install_google_cloud_sdk \
      'Oracle Cloud Infrastructure (requires Python)' install_oci_cli \
      'act' install_act \
      'circleci-cli' install_circleci_cli \
      'The Travis Client' install_travis_client
    tui_set_quit_option d 'Done'

    if ! tui_select_option 'Enter your option'; then
      break
    fi
  done
}

install_aws_shell() {
  pip_install system aws-shell
}

install_google_cloud_sdk() {
  if is_macos; then
    brew_cask_install google-cloud-sdk
  elif is_arch; then
    trizen_sync google-cloud-sdk
  else
    unsupported_platform_error
  fi
}

install_oci_cli() {
  pip_install system oci-cli
}

install_act() {
  if is_macos; then
    brew_install nektos/tap/act
  elif is_arch; then
    trizen_sync act
  else
    unsupported_platform_error
  fi
}

install_circleci_cli() {
  if is_macos; then
    brew_install circleci
  elif [[ ! -x /usr/local/bin/circleci ]]; then
    curl -fLSs https://circle.ci/cli | sudo bash
  fi
}

install_travis_client() {
  gem_install system travis
}
