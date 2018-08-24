# utility.bash --- Utility functions for the install script
# author: Seong Yong-ju <sei40kr@gmail.com>

NC="\033[0m"
RED="\033[0;31m"
YELLOW="\033[0;33m"
BLUE="\033[0;34m"

progress() {
    local message="$1"

    echo -e "${BLUE}===>${NC} ${message}"
}

warn() {
    local message="$1"

    echo -e "${YELLOW}Warning${NC}: ${message}" >&2
}

die() {
    local message="$1"
    local err_code="${2:-1}"

    echo -e "${RED}Error${NC}: ${message}" >&2
    exit "$err_code"
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
