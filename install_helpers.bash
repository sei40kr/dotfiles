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

read_yn() {
    local prompt="$1"

    local answer
    read -n1 -rp "${prompt} [yn]: " answer
    echo ''
    echo ''

    if [[ "$answer" == [yY] ]]; then
        return 0
    fi

    return 1
}

print_info() {
    local message="$1"

    echo "${message}"
}

print_important() {
    local message="$1"

    echo -e "${BOLD}${message}${RESET}"
}

print_warning() {
    local message="$1"

    echo -e "${YELLOW}${message}${RESET}"
}

print_danger() {
    local message="$1"

    echo -e "${RED}${message}${RESET}"
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

with_spinner() {
    "$@" 1>/dev/null 2>/dev/null &
    local pid="$!"

    echo '  '

    local i=0
    while kill -0 "$pid" 2>/dev/null; do
        echo -ne "\b${SPINNER:i++%${#SPINNER}:1}"
        sleep 0.25
    done

    echo -e '\b\b'

    wait "$pid"
    if [[ "$?" != 0 && "$?" != 255 ]]; then
        return $?
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

pacman_query() {
    local -a pkgs=( "$@" )

    pacman -Q "${pkgs[@]}" &>/dev/null
}

pacman_sync() {
    local -a pkgs=( "$@" )

    echo 'Installing'

    for pkg in "${pkgs[@]}"; do
        echo "- ${pkg}"
    done

    with_spinner sudo pacman -Sy --needed --noconfirm --noprogressbar "${pkgs[@]}"
}

trizen_sync() {
    local -a pkgs=( "$@" )

    if ! pacman_query trizen; then
        error 'trizen is not installed. Aborting.'
    fi

    echo 'Installing'

    for pkg in "${pkgs[@]}"; do
        echo "- ${pkg}"
    done

    with_spinner trizen -Sy --needed --noconfirm --noprogressbar --nopull "${pkgs[@]}"
}

stack_install() {
    local -a pkgs=( "$@" )

    if ! pacman_query stack; then
        error 'stack is not installed. Aborting.'
    fi

    echo 'Installing'
    printf "%s\n" "${pkgs[@]}"

    with_spinner stack install "${pkgs[@]}"
}

systemctl_enable() {
    local service="$1"

    sudo systemctl enable --now "$service"
}

systemctl_mask() {
    local service="$1"

    sudo systemctl mask --now "$service"
}

systemctl_user_enable() {
    local service="$1"

    systemctl --user enable --now "$service"
}

rustup_toolchain_install() {
    local toolchain="$1"

    if ! pacman_query rustup; then
        error 'rustup is not installed. Aborting.'
    fi

    with_spinner rustup toolchain install "$toolchain"
}

rustup_component_add() {
    local toolchain="$1"
    shift
    local components=( "$@" )

    if ! pacman_query rustup; then
        error 'rustup is not installed. Aborting.'
    fi

    with_spinner rustup component add --toolchain "$toolchain" "${components[@]}"
}

goenv_install() {
    local go_version="$1"

    # TODO check goenv executable exists

    with_spinner "${GOENV_ROOT}/bin/goenv" install -s "$go_version"
}

go_get() {
    local go_version
    local -a pkgs
    local go_exec
    go_version="$1"
    shift
    pkgs=( "$@" )

    if [[ "$go_version" == system ]]; then
        go_exec=/usr/bin/go
    else
        go_exec="${GOENV_ROOT}/versions/${go_version}/go"
    fi

    # TODO check go executable exists

    with_spinner GOPATH="${HOME}/go/${go_version}" "$go_exec" get -u "${pkgs[@]}"
}

pyenv_install() {
    local python_version="$1"

    # TODO check pyenv executable exists

    with_spinner "${PYENV_ROOT}/bin/pyenv" install -s "$python_version"
}

pip_install() {
    local python_version="$1"
    shift
    local -a pkgs=( "$@" )
    local -a pip_opts=( -q -U )

    local pip_exec
    if [[ "$python_version" == system ]]; then
        pip_exec=/usr/bin/pip
        pip_opts+=( --user )
    else
        pip_exec="${PYENV_ROOT}/versions/${python_version}/bin/pip"
    fi

    # TODO check pip executable exists

    with_spinner "$pip_exec" --disable-pip-version-check install "${pip_opts[@]}" "${pkgs[@]}"
}

rbenv_install() {
    local ruby_version="$1"

    # TODO check rbenv executable exists

    with_spinner "${RBENV_ROOT}/bin/rbenv" install -s "$ruby_version"
}

gem_install() {
    local ruby_version="$1"
    shift
    local -a gems=( "$@" )
    local -a gem_opts=( -q --silent --norc )

    local gem_exec
    if [[ "$ruby_version" == system ]]; then
        gem_exec=/usr/bin/gem
        gem_opts+=( --user-install )
    else
        gem_exec="${RBENV_ROOT}/versions/${ruby_version}/bin/gem"
    fi

    # TODO check gem executable exists

    with_spinner "$gem_exec" install "${gem_opts[@]}" "${gems[@]}"
}

nvm_install() {
    local node_version="$1"

    # TODO check nvm exists

    ( . "${NVM_DIR}/nvm.sh"
      with_spinner nvm install --no-progress "$node_version" )
}

yarn_global_add() {
    local node_version="$1"
    shift
    local -a pkgs=( "$@" )

    local node_execdir
    if [[ "$node_version" == system ]]; then
      node_execdir=/usr/bin
    else
      node_execdir="${NVM_DIR}/versions/${node_version}/bin"
    fi

    # TODO check node and yarn executables exist

    PATH="${node_execdir}:${PATH}" with_spinner yarn global add --no-default-rc --noprogress --non-interactive "${pkgs[@]}"
}

r_install() {
    local -a pkgs
    pkgs=( "$@" )

    with_spinner R --vanilla \
      -e "args<-commandArgs(trailingOnly=T);options(repos=args[1]);install.packages(args[-1])" \
      -q \
      --args 'https://cran.ism.ac.jp' "${pkgs[@]}"
}
