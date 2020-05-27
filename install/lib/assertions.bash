# author: Seong Yong-ju <sei40kr@gmail.com>

# assert_not_empty ACTUAL [ERROR]
#
# Assert ACTUAL is not empty. If empty, the process exits with status code 1.
#
assert_not_empty() {
  local actual="$1"
  local error="$2"

  if [[ "$actual" == '' ]]; then
    assertion_error "$error"
  fi
}

# assert_equal ACTUAL EXPECTED [ERROR]
#
# Assert ACTUAL equals to EXPECTED. If not, the process exits with status code
# 1.
#
assert_equal() {
  local actual="$1"
  local expected="$2"
  local error="$3"

  if [[ ! "$actual" == "$expected" ]]; then
    assertion_error \
      "${error:-Assertion failed: ${actual} == ${expected}. Aborting.}"
  fi
}

# assert_archlinux
#
# Assert the running platform is Arch Linux. If not, the process exits with
# status code 1.
#
assert_archlinux() {
  if ! is_archlinux; then
    unsupported_platform_error
  fi
}

# assert_macos
#
# Assert the running platform is macOS. If not, the process exits with
# status code 1.
#
assert_macos() {
  if ! is_macos; then
    unsupported_platform_error
  fi
}

# assert_command_exists COMMAND [ERROR]
#
# Assert a command exists. If the command is not found, the process exits with
# status code 127.
#
assert_command_exists() {
  local command="$1"
  local error="$2"

  if ! command_exists "$command"; then
    tui-error "${error:-${command} is not found. Aborting.}"
    exit 127
  fi
}

# assert_command_exists FILE [ERROR]
#
# Assert a file exists and is executable. If not, the process exits with status
# code 127.
#
assert_executable() {
  local file="$1"
  local error="$2"

  if [[ ! -x "$file" ]]; then
    tui-error "${error:-$(abbreviate_filepath "$file") is not found or not executable. Aborting.}"
    exit 127
  fi
}
