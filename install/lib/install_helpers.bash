# install_helpers.bash
# author: Seong Yong-ju <sei40kr@gmail.com>

SPINNER='/-\|'

COLUMNS="$(tput cols)"

BOLD="$(tput bold)"
RED="$(tput setaf 1)"
GREEN="$(tput setaf 2)"
YELLOW="$(tput setaf 3)"
BLUE="$(tput setaf 4)"
PURPLE="$(tput setaf 5)"
CYAN="$(tput setaf 6)"
WHITE="$(tput setaf 7)"
RESET="$(tput sgr0)"


is_arch() {
    [[ "$OSTYPE" == linux* && -f /etc/arch-release ]]
}

is_macos() {
    [[ "$OSTYPE" == darwin* ]]
}


print_line() {
    local empty_line="$(printf "%${COLUMNS}s")"

    echo "${empty_line// /-}"
}

print_title() {
    local title="$1"

    clear
    print_line
    echo "# ${BOLD}${title}${RESET}"
    print_line
    echo ''
}

print_desc() {
    local desc="$1"

    echo "$desc"
    echo ''
}

menu_item() {
    local title="$1"

    echo "${BOLD}${title}${RESET}"
}

pacman_menu_item() {
    local title="$1"
    shift
    local -a pkgs=( "$@" )

    if pacman -Q "${pkgs[@]}" 1>/dev/null 2>/dev/null; then
        echo "${GREEN}☑${RESET} ${BOLD}${title}${RESET}"
    else
        echo "☐ ${BOLD}${title}${RESET}"
    fi
}

error() {
    local message="$1"
    local status_code="$2"

    echo "$message" >&2
    exit "${status_code:-1}"
}

pause() {
    print_line
    read -sn 1 -p 'Press enter to continue ...'
}
