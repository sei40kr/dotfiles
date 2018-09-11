# systemctl.bash --- systemctl facade
# author: Seong Yong-ju <sei40kr@gmail.com>

__systemctl_svcs=()
__systemctl_usrsvcs=()

systemctl_enable_facade() {
    local svc="$1"

    __systemctl_svcs+=( "$svc" )
}

systemctl_enable_user_facade() {
    local usrsvc="$1"

    __systemctl_usrsvcs+=( "$usrsvc" )
}


systemctl_reduce_svcs() {
    [[ "${#__systemctl_svcs[@]}" == 0 ]] && return

    ! hash systemctl 2>/dev/null && die 'systemctl is not found.'

    log_wait 'Enabling systemctl services ...'

    wrap_facade_cmd sudo systemctl enable --now "${__systemctl_svcs[@]}"
}

register_facade_reducer systemctl_reduce_svcs

systemctl_reduce_usrsvcs() {
    [[ "${#__systemctl_usrsvcs[@]}" == 0 ]] && return

    log_wait 'Enabling systemctl user services ...'

    wrap_facade_cmd systemctl enable --user --now "${__systemctl_usrsvcs[@]}"
}

register_facade_reducer systemctl_reduce_usrsvcs
