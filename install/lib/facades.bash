# facades.bash
# author: Seong Yong-ju

## Pacman & Trizen

pacman_sync() {
    assert_archlinux
    assert_command_exists pacman

    local -a packages=( "$@" )

    for package in "${packages[@]}"; do
        print-list-item "Installing ${package}"
    done

    run_process sudo pacman -Sy --needed --noconfirm --noprogressbar "${packages[@]}"
}

trizen_sync() {
    assert_archlinux
    assert_command_exists trizen

    local -a packages=( "$@" )

    for package in "${packages[@]}"; do
        print-list-item "Installing ${package}"
    done

    run_process trizen -Sy --needed --noconfirm --noprogressbar --nopull "${packages[@]}"
}


## Homebrew

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
    assert_macos
    assert_command_exists brew

    local formula
    # Install options with double quotes
    local -a options

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
    assert_macos
    assert_command_exists brew

    {
        echo 'cask_args appdir: "/Applications"'

        for cask in "$@"; do
            echo "cask \"${cask}\""
        done
    } | brew bundle --file=-
}


## systemctl

systemctl_enable() {
    assert_command_exists systemctl

    local service="$1"

    print-step "Enabling system service ${service}"

    sudo systemctl enable --now "$service"
}

systemctl_mask() {
    assert_command_exists systemctl

    local service="$1"

    print-step "Masking system service ${service}"

    sudo systemctl mask --now "$service"
}

systemctl_user_enable() {
    assert_command_exists systemctl

    local service="$1"

    print-step "Enabling user service ${service}"

    systemctl --user enable --now "$service"
}


## Rustup

rustup_toolchain_install() {
    assert_command_exists rustup

    local toolchain="$1"

    print-step "Installing Rust ${toolchain} toolchain"

    run_process rustup toolchain install "$toolchain"
}

rustup_component_add() {
    assert_command_exists rustup

    local toolchain="$1"
    shift
    local components=( "$@" )

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

goenv_install() {
    assert_executable "${GOENV_ROOT}/bin/goenv"

    local go_version="$1"

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

stack_install() {
    assert_command_exists stack

    local -a packages=( "$@" )

    for package in "${packages[@]}"; do
        print-list-item "Installing ${package}"
    done

    run_process stack install "${packages[@]}"
}


## pyenv

pyenv_install() {
    assert_executable "${PYENV_ROOT}/bin/pyenv"

    local python_version="$1"

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

        assert_executable "$pip_exec" \
            'pip executable of system-installed version not found. Aborting.'

        pip_opts+=( --user )
    else
        pip_exec="${PYENV_ROOT}/versions/${python_version}/bin/pip"

        assert_executable "$pip_exec" \
            "pip executable of ${python_version} not found. Aborting."
    fi

    for package in "${packages[@]}"; do
        print-list-item "Installing ${package}"
    done

    run_process "$pip_exec" --disable-pip-version-check install "${pip_opts[@]}" "${packages[@]}"
}


## rbenv

rbenv_install() {
    assert_executable "${RBENV_ROOT}/bin/rbenv"

    local ruby_version="$1"

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

        assert_executable "$gem_exec" \
            'gem executable of system-installed version not found. Aborting.'

        gem_opts+=( --user-install )
    else
        gem_exec="${RBENV_ROOT}/versions/${ruby_version}/bin/gem"

        assert_executable "$gem_exec" \
            "gem executable of ${ruby_version} not found. Aborting."
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

yarn_global_add() {
    assert_command_exists yarn

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

        assert_executable "${node_exec_dir}/node" \
            'node executable of system-installed version not found. Aborting.'
    else
        node_exec_dir="${NVM_DIR}/versions/${node_version}/bin"

        assert_executable "$node_exec_dir" \
            "node executable of ${node_version} not found. Aborting."
    fi

    for package in "${packages[@]}"; do
        print-list-item "Installing ${package}${system_node:- to Node.js ${node_version}}"
    done

    run_process PATH="${node_exec_dir}:${PATH}" yarn global add --no-default-rc --noprogress --non-interactive "${packages[@]}"
}


## R

r_install() {
    assert_command_exists R

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
