# facades.bash
# author: Seong Yong-ju

## Pacman & Trizen

__verify_pacman() {
    if ! is_arch; then
        tui-error 'Pacman facades must be called only on Arch Linux. Aborting.'
        exit 1
    fi

    if ! command_exists pacman; then
        tui-error 'pacman not found. Aborting.'
        exit 1
    fi
}

pacman_sync() {
    local -a packages=( "$@" )

    __verify_pacman

    for package in "${packages[@]}"; do
        print-list-item "Installing ${package}"
    done

    run_process sudo pacman -Sy --needed --noconfirm --noprogressbar "${packages[@]}"
}

__verify_trizen() {
    if [[ ! -f /etc/arch-release ]]; then
        tui-error 'Trizen facades must be called only on Arch Linux. Aborting.'
        exit 1
    fi

    if ! command_exists trizen; then
        tui-error 'trizen not found. Aborting.'
        exit 1
    fi
}

trizen_sync() {
    local -a packages=( "$@" )

    __verify_trizen

    for package in "${packages[@]}"; do
        print-list-item "Installing ${package}"
    done

    run_process trizen -Sy --needed --noconfirm --noprogressbar --nopull "${packages[@]}"
}


## Homebrew

__verify_homebrew() {
    if ! is_macos; then
        tui-error 'Homebrew facades must be called only on macOS. Aborting.'
        exit 1
    fi

    if ! hash brew 2>/dev/null; then
        # shellcheck disable=SC2016
        tui-error 'Homebrew not found. Run `/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"` to install Homebrew. Aborting.'
        exit 1
    fi
}


# brew_install FORMULA [OPTION ...] ...
#
# Install Homebrew formulas.
#
# Examples:
# brew_install zsh tmux
# brew_install \
#     d12frosted/emacs-plus --with-emacs-27-branch --without-spacemacs-icon \
#     libvterm cmake
#
brew_install() {
    local formula
    # Install options with double quotes
    local -a options

    __verify_homebrew

    function print_brewfile_line() {
        local IFS=','
        echo "brew \"${formula}\"${options:+, [${options[*]}]}"
    }

    {
        for arg in "$@"; do
            if [[ "$arg" == --* ]]; then
                options+=( "\"${arg#--}\"" )
            else
                if [[ -n "$formula" ]]; then
                    print_brewfile_line
                fi

                formula="$arg"
                options=()
            fi
        done

        if [[ -n "$formula" ]]; then
            print_brewfile_line
        fi
    } | brew bundle --file=-
}

# brew_cask_install CASK ...
#
# Install Homebrew casks.
#
# Example:
# brew_cask_install google-chrome
#
brew_cask_install() {
    __verify_homebrew

    {
        echo 'cask_args appdir: "/Applications"'

        for cask in "$@"; do
            echo "cask \"${cask}\""
        done
    } | brew bundle --file=-
}


## systemctl

__verify_systemctl() {
    if ! command_exists systemctl; then
        tui-error 'systemctl not found. Aborting.'
        exit 1
    fi
}

systemctl_enable() {
    local service="$1"

    __verify_systemctl

    print-step "Enabling system service ${service}"

    sudo systemctl enable --now "$service"
}

systemctl_mask() {
    local service="$1"

    __verify_systemctl

    print-step "Masking system service ${service}"

    sudo systemctl mask --now "$service"
}

systemctl_user_enable() {
    local service="$1"

    __verify_systemctl

    print-step "Enabling user service ${service}"

    systemctl --user enable --now "$service"
}


## Rustup

__verify_rustup() {
    if ! command_exists rustup; then
        tui-error 'rustup not found. Aborting.'
        exit 1
    fi
}

rustup_toolchain_install() {
    local toolchain="$1"

    __verify_rustup

    print-step "Installing Rust ${toolchain} toolchain"

    run_process rustup toolchain install "$toolchain"
}

rustup_component_add() {
    local toolchain="$1"
    shift
    local components=( "$@" )

    __verify_rustup

    local stable_toolchain
    if [[ "$toolchain" == stable ]]; then
        stable_toolchain=1
    fi

    for component in "${components[@]}"; do
        print-step "Installing ${component}${stable_toolchain:- to Rust ${toolchain}}"
    done

    run_process rustup component add --toolchain "$toolchain" "${components[@]}"
}


## goenv

__verify_goenv() {
    if [[ ! -x "${GOENV_ROOT}/bin/goenv" ]]; then
        if command_exists goenv; then
            tui-error 'goenv must not be installed with a system package manager. Aborting.'
        else
            tui-error 'goenv not found. Aborting.'
        fi

        exit 1
    fi
}

goenv_install() {
    local go_version="$1"

    __verify_goenv

    print-step "Installing Go v${go_version}"

    run_process "${GOENV_ROOT}/bin/goenv" install -s "$go_version"
}


## Go

__go_exec() {
    local version="$1"

    if [[ "$version" == system ]]; then
        if is_macos; then
            echo /usr/local/bin/go
        else
            echo /usr/bin/go
        fi
    else
        echo "${GOENV_ROOT}/versions/${version}/go"
    fi
}

go_get() {
    local go_version="$1"
    shift
    local packages=( "$@" )

    local go_exec="$(__go_exec "$go_version")"

    if [[ ! -x "$go_exec" ]]; then
        if [[ "$go_version" == system ]]; then
            tui-error "go executable of ${go_version} not found. Aborting."
        else
            tui-error 'go executable of system-installed version not found. Aborting.'
        fi

        exit 1
    fi

    local system_go
    if [[ "$go_version" == system ]]; then
        system_go=1
    fi

    for package in "${packages[@]}"; do
        local short_name
        short_name="${package##*/}"
        short_name="${package%%@*}"
        print-list-item "Installing ${short_name}${system_go:- to Go ${go_version}}"
    done

    run_process GOPATH="${HOME}/go/${go_version}" "$go_exec" get -u "${packages[@]}"
}


## Haskell Tool Stack

__verify_stack() {
    if ! command_exists stack; then
        tui-error 'stack not installed. Aborting.'
        exit 1
    fi
}

stack_install() {
    local -a packages=( "$@" )

    __verify_stack

    for package in "${packages[@]}"; do
        print-list-item "Installing ${package}"
    done

    run_process stack install "${packages[@]}"
}


## pyenv

__verify_pyenv() {
    if [[ ! -x "${PYENV_ROOT}/bin/pyenv" ]]; then
        if command_exists pyenv; then
            tui-error 'pyenv must not be installed with a system package manager. Aborting.'
        else
            tui-error 'pyenv not found. Aborting.'
        fi

        exit 1
    fi
}

pyenv_install() {
    local python_version="$1"

    __verify_pyenv

    print-step "Installing Python v${python_version}"

    run_process "${PYENV_ROOT}/bin/pyenv" install -s "$python_version"
}


## pip

pip_install() {
    local python_version="$1"
    shift
    local -a packages=( "$@" )
    local -a pip_opts=( -q -U )

    local pip_exec
    if [[ "$python_version" == system ]]; then
        if is_macos; then
            pip_exec=/usr/local/bin/pip
        else
            pip_exec=/usr/bin/pip
        fi

        if [[ ! -x "$pip_exec" ]]; then
            tui-error 'pip executable of system-installed version not found. Aborting.'
            exit 1
        fi

        pip_opts+=( --user )
    else
        pip_exec="${PYENV_ROOT}/versions/${python_version}/bin/pip"

        if [[ ! -x "$pip_exec" ]]; then
            tui-error "pip executable of ${python_version} not found. Aborting."
            exit 1
        fi
    fi

    for package in "${packages[@]}"; do
        print-list-item "Installing ${package}"
    done

    run_process "$pip_exec" --disable-pip-version-check install "${pip_opts[@]}" "${packages[@]}"
}


## rbenv

__verify_rbenv() {
    if [[ ! -x "${RBENV_ROOT}/bin/rbenv" ]]; then
        if command_exists rbenv; then
            tui-error 'rbenv must not be installed with a system package manager. Aborting.'
        else
            tui-error 'rbenv not found. Aborting.'
        fi

        exit 1
    fi
}

rbenv_install() {
    local ruby_version="$1"

    __verify_rbenv

    print-step "Installing Ruby v${ruby_version}"

    run_process "${RBENV_ROOT}/bin/rbenv" install -s "$ruby_version"
}


## gem

gem_install() {
    local ruby_version="$1"
    shift
    local -a gems=( "$@" )
    local -a gem_opts=( -q --silent --norc )

    local system_ruby
    local gem_exec
    if [[ "$ruby_version" == system ]]; then
        system_ruby=1

        if is_macos; then
            gem_exec=/usr/local/bin/gem
        else
            gem_exec=/usr/bin/gem
        fi

        if [[ ! -x "$gem_exec" ]]; then
            tui-error 'gem executable of system-installed version not found. Aborting.'
            exit 1
        fi

        gem_opts+=( --user-install )
    else
        gem_exec="${RBENV_ROOT}/versions/${ruby_version}/bin/gem"

        if [[ ! -x "$gem_exec" ]]; then
            tui-error "gem executable of ${ruby_version} not found. Aborting."
            exit 1
        fi
    fi

    for gem in "${gems[@]}"; do
        print-list-item "Installing ${gem}${system_ruby:- to Ruby ${ruby_version}}"
    done

    run_process "$gem_exec" install "${gem_opts[@]}" "${gems[@]}"
}


## nvm

__verify_nvm() {
    if [[ ! -f "${NVM_DIR}/nvm.sh" ]]; then
        tui-error 'nvm not found. Aborting.'
        exit 1
    fi
}

nvm_install() {
    local node_version="$1"

    __verify_nvm

    print-list-item "Installing Node.js ${node_version}"

    ( . "${NVM_DIR}/nvm.sh"
      run_process nvm install --no-progress "$node_version" )
}


## Yarn

__verify_yarn() {
    if ! command_exists yarn; then
        tui-error 'yarn not found. Aborting.'
        exit 1
    fi
}

yarn_global_add() {
    local node_version="$1"
    shift
    local -a packages=( "$@" )

    local system_node
    local node_exec_dir
    if [[ "$node_version" == system ]]; then
        system_node=1

        if is_macos; then
            node_exec_dir=/usr/local/bin/node
        else
            node_exec_dir=/usr/bin/node
        fi

        if [[ ! -x "${node_exec_dir}/node" ]]; then
            tui-error 'node executable of system-installed version not found. Aborting.'
            exit 1
        fi
    else
        node_exec_dir="${NVM_DIR}/versions/${node_version}/bin"

        if [[ ! -x "${node_exec_dir}/node" ]]; then
            tui-error "node executable of ${node_version} not found. Aborting."
            exit 1
        fi
    fi

    __verify_yarn

    for package in "${packages[@]}"; do
        print-list-item "Installing ${package}${system_node:- to Node.js ${node_version}}"
    done

    run_process PATH="${node_exec_dir}:${PATH}" yarn global add --no-default-rc --noprogress --non-interactive "${packages[@]}"
}


## R

__verify_r() {
    if ! command_exists R; then
        tui-error 'R not found. Aborting.'
        exit 1
    fi
}

r_install() {
    local -a packages
    packages=( "$@" )

    for package in "${packages[@]}"; do
        print-list-item "Installing ${package}"
    done

    run_process R --vanilla \
        -e "args<-commandArgs(trailingOnly=T);options(repos=args[1]);install.packages(args[-1])" \
        -q \
        --args 'https://cran.ism.ac.jp' "${packages[@]}"
}
