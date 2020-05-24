# author: Seong Yong-ju <sei40kr@gmail.com>

# assert_archlinux
#
# Assert the running platform is Arch Linux. If not, the process exits with
# status code 1.
#
assert_archlinux() {
    if ! is_arch; then
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
