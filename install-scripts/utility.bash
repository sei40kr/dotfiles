# utility.bash --- Utility functions for the install script
# author: Seong Yong-ju <sei40kr@gmail.com>

RED="\033[0;31m"
GREEN="\033[0;32m"
YELLOW="\033[0;33m"
BLUE="\033[0;34m"
MAGENTA="\033[0;35m"
CYAN="\033[0;36m"

BOLD="\033[1m"

NC="\033[0m"


is_verbose() {
    [[ "$is_verbose" == 1 ]]
}

do_update() {
    [[ "$do_update" == 1 ]]
}

is_dry_run() {
    [[ "$is_dry_run" == 1 ]]
}


log_wait() {
    local msg="$1"

    echo -e "${BLUE}==>${NC} ${BOLD}${msg}${NC}"
}

log_warn() {
    local msg="$1"

    echo -e "${YELLOW}WARNING:${NC} ${msg}" >&2
}

die() {
    local msg="$1"
    local errcode="${2:-1}"

    echo -e "${RED}ERROR:${NC} ${msg}" >&2
    exit "$errcode"
}


is_arch() {
    [[ -f '/etc/arch-release' ]]
}

is_macos() {
    [[ "$OSTYPE" == darwin* ]]
}

is_linux() {
    [[ "$OSTYPE" == linux* ]]
}


__facade_reducers=()

register_facade_reducer() {
    local reducer="$1"

    __facade_reducers+=( "$reducer" )
}

run_all_facade_reducers() {
    for reducer in "${__facade_reducers[@]}"; do
        eval "$reducer"
    done
}


wrap_facade_cmd() {
    local cmd="$@"

    if is_dry_run || is_verbose; then
        echo ">" "${cmd[@]}"
    fi
}
