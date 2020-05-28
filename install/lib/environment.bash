# author: Seong Yong-ju

# is_archlinux
#
is_archlinux() {
  [[ "$OSTYPE" == linux* && -f /etc/arch-release ]]
}

# is_macos
#
is_macos() {
  [[ "$OSTYPE" == darwin* ]]
}

# unsupported_platform_error
#
unsupported_platform_error() {
  tui-error "The platform you're running is not supported."
  exit 1
}

# command_exists COMMAND
#
command_exists() {
  local command="$1"

  hash "$command" 2>/dev/null
}
