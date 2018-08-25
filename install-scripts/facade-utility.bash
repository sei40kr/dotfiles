# facade-utility.bash --- Utility functions for facades
# author: Seong Yong-ju <sei40kr@gmail.com>

facade_exec_cmd() {
    is_verbose && echo ">" "$@"

    if ! do_dry_run; then
        command "$@" 1>/dev/null

        # If the command didn't complete successfully
        if [[ "$?" != 0 ]]; then
            is_verbose || echo ">" "$@" >&2
            die 'An error occured while installation.'
        fi
    fi
}
