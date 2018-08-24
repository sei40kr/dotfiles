# facade-utility.bash --- Utility functions for facades
# author: Seong Yong-ju <sei40kr@gmail.com>

NC="\033[0m"
BLUE="\033[0;34m"

facade_exec_cmd() {
    echo -e "${BLUE}===>${NC}" "$@"

    if ! do_dry_run; then
        command "$@" 1>/dev/null
    fi
}
