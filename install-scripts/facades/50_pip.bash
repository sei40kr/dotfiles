# pip.bash --- pip facade
# author: Seong Yong-ju <sei40kr@gmail.com>

__pip3_pkgs=()
__pip2_pkgs=()

pip3_install_facade() {
    local pkg="$1"

    __pip3_pkgs+=( "$pkg" )
}

pip2_install_facade() {
    local pkg="$1"

    __pip2_pkgs+=( "$pkg" )
}


pip3_reduce_pkgs() {
    [[ "${#__pip3_pkgs[@]}" == 0 ]] && return

    ! hash pip3 2>/dev/null && die 'pip3 is not found.'

    log_wait 'Enabling pip3 packages ...'

    if do_update; then
        wrap_facade_cmd pip3 install -U "${__pip3_pkgs[@]}"
    else
        wrap_facade_cmd pip3 install "${__pip3_pkgs[@]}"
    fi
}

register_facade_reducer pip3_reduce_pkgs

pip2_reduce_pkgs() {
    [[ "${#__pip2_pkgs[@]}" == 0 ]] && return

    ! hash pip2 2>/dev/null && die 'pip2 is not found.'

    log_wait 'Enabling pip2 packages ...'


    if do_update; then
        wrap_facade_cmd pip2 install -U "${__pip2_pkgs[@]}"
    else
        wrap_facade_cmd pip2 install "${__pip2_pkgs[@]}"
    fi
}

register_facade_reducer pip2_reduce_pkgs
