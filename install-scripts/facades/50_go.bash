# go.bash --- go facade
# author: Seong Yong-ju <sei40kr@gmail.com>

__go_pkgs=()

go_get_facade() {
    local pkg="$1"

    __go_pkgs+=( "$pkg" )
}


go_get_facade_reducer() {
    [[ "${#__go_pkgs[@]}" == 0 ]] && return

    # TODO Install go if not found
    ! hash go 2>/dev/null && die 'go is not found.'
    [[ "$GOPATH" == "" ]] && die '$GOPATH is not set.'

    log_wait 'Installing Go packages ...'

    if do_update; then
        wrap_facade_cmd go get -u "${__go_pkgs[@]}"
    else
        wrap_facade_cmd go get "${__go_pkgs[@]}"
    fi
}

register_facade_reducer go_get_facade_reducer
