# stack.bash --- Haskell Tool Stack facade
# author: Seong Yong-ju <sei40kr@gmail.com>

stack_pkgs=()
stack_install_flags=()

# USAGE
#    stack_install_pkg TARGET [FLAG...]
#
# EXAMPLES
#    stack_install_pkg xmonad
#    stack_install_pkg xmobar with_dbus with_alsa
#
stack_install_pkg() {
    local pkg="$1"
    shift
    local flags=( "$@" )

    stack_pkgs+=( "$pkg" )
    for flag in "${flags[@]}"; do
        stack_install_flags+=( "${pkg}:${flag}" )
    done
}

stack_reduce_pkgs() {
    hash stack 2>/dev/null || die 'stack is not found.'

    progress 'Installing stack packages ...'

    if [[ "${#stack_pkgs}" == 0 ]]; then
        warn "There're no stack packages to install."
        return
    fi

    local install_opts=()
    for flag in "${stack_install_flags[@]}"; do
        install_opts+=( --flag "$flag" )
    done

    facade_exec_cmd stack install "${stack_pkgs[@]}" "${install_opts[@]}"
}

facade_add_reducer stack_reduce_pkgs
