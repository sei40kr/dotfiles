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

pause() {
    print_line
    read -sn 1 -p 'Press enter to continue ...'
}
