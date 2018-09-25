# pip.bash --- pip facade
# author: Seong Yong-ju <sei40kr@gmail.com>

__pip3_install_pkgs=()
__pip2_install_pkgs=()

pip3_install_facade() {
    local pkg="$1"

    __pip3_install_pkgs+=( "$pkg" )
}

pip2_install_facade() {
    local pkg="$1"

    __pip2_install_pkgs+=( "$pkg" )
}


pip3_install_facade_reducer() {
    [[ "${#__pip3_install_pkgs[@]}" == 0 ]] && return

    ! hash pip3 2>/dev/null && die 'pip3 is not found.'

    log_wait 'Installing pip3 packages ...'

    if do_update; then
        wrap_facade_cmd pip3 install -U "${__pip3_install_pkgs[@]}"
    else
        wrap_facade_cmd pip3 install "${__pip3_install_pkgs[@]}"
    fi
}

register_facade_reducer pip3_install_facade_reducer

pip2_install_facade_reducer() {
    [[ "${#__pip2_install_pkgs[@]}" == 0 ]] && return

    ! hash pip2 2>/dev/null && die 'pip2 is not found.'

    log_wait 'Installing pip2 packages ...'


    if do_update; then
        wrap_facade_cmd pip2 install -U "${__pip2_install_pkgs[@]}"
    else
        wrap_facade_cmd pip2 install "${__pip2_install_pkgs[@]}"
    fi
}

register_facade_reducer pip2_install_facade_reducer
