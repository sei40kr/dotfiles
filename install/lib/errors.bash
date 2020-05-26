# author: Seong Yong-ju <sei40kr@gmail.com>

assertion_error() {
  local error="$1"

  tui-error "${error:-Assertion failed. Aborting.}"
  exit 1
}

unsupported_platform_error() {
    tui-error "The platform you're running is not supported."
    exit 1
}
