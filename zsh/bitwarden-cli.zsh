# bitwarden-cli.zsh
# author: Seong Yong-ju <sei40kr@gmail.com>

bw() {
    if [[ "${+commands[bw]}" == 0 ]]; then
        echo 'Error: bw command not found. Aborting.' >&2
        return 127
    fi

    if [[ -z "$BW_SESSION" ]]; then
        if command bw login --check &>/dev/null; then
            BW_SESSION="$(command bw unlock --raw)"
        else
            BW_SESSION="$(command bw login --raw)"
        fi

        local exit_code="$?"
        if [[ "$exit_code" != 0 ]]; then
            return "$exit_code"
        fi

        export BW_SESSION
    fi

    command bw "$@"
}
