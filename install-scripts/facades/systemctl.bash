# systemctl.bash --- systemctl facade
# author: Seong Yong-ju <sei40kr@gmail.com>

SYSTEMCTL_ENABLE_OPTS=( --now )
SYSTEMCTL_ENABLE_USER_OPTS=( --user --now )

systemctl_svcs=()
systemctl_usrsvcs=()

systemctl_enable_svc() {
    systemctl_svcs=( "${systemctl_svcs[@]}" "$1" )
}

systemctl_enable_usrsvc() {
    systemctl_usrsvcs=( "${systemctl_usrsvcs[@]}" "$1" )
}


## Reducers

systemctl_reduce_svcs() {
    # If there're no services to enable, do nothing
    if [[ "${#systemctl_svcs}" == 0 ]]; then
        return
    fi

    facade_exec_cmd sudo systemctl enable "${SYSTEMCTL_ENABLE_OPTS[@]}" "${systemctl_svcs[@]}"
}

facade_add_reducer systemctl_reduce_svcs

systemctl_reduce_usrsvcs() {
    # If there're no user services to enable, do nothing
    if [[ "${#systemctl_usrsvcs}" == 0 ]]; then
        return
    fi

    facade_exec_cmd systemctl enable "${SYSTEMCTL_ENABLE_USER_OPTS[@]}" "${systemctl_usrsvcs[@]}"
}

facade_add_reducer systemctl_reduce_usrsvcs
