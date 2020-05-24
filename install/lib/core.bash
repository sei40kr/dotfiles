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

abbreviate_filepath() {
    local filepath="$1"

    # Make sure $HOME is at the beginning
    if [[ "$filepath" == "$HOME"* ]]; then
        filepath="${filepath/${HOME}/~}"
    fi

    echo "$filepath"
}
