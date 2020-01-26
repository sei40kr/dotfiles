# env.bash
# author: Seong Yong-ju

is_arch() {
    [[ "$OSTYPE" == linux* && -f /etc/arch-release ]]
}

is_macos() {
    [[ "$OSTYPE" == darwin* ]]
}

command_exists() {
    local command="$1"

    hash "$command" 2>/dev/null
}
