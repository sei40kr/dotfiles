# process.bash
# author: Seong Yong-ju

run_process() {
    local command=( "$@" )

    local tmp_file
    if ! tmp_file="$(mktemp)"; then
        tui-error 'Failed to create a temporary file. Aborting.'
    fi

    eval "${command[@]}" 1>/dev/null 2>"$tmp_file"

    local exit_code="$?"
    if [[ "$exit_code" != 0 ]]; then
        local IFS=' '
        tui-error "\`${command[*]//[$'\r\n']}\` exited with code ${exit_code}:\n$(cat "$tmp_file")"
    fi

    rm -f "$tmp_file"

    if [[ "$exit_code" != 0 ]]; then
        exit "$exit_code"
    fi
}
