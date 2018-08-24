# facade-utility.bash --- Utility functions for facades
# author: Seong Yong-ju <sei40kr@gmail.com>

facade_exec_cmd() {
    is_verbose || do_dry_run && echo ">" "$@"

    if ! do_dry_run; then
        command "$@" 1>/dev/null
    fi
}
