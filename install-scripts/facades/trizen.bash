# trizen.bash --- trizen facade
# author: Seong Yong-ju <sei40kr@gmail.com>

TRIZEN_SYNC_OPTS=( -S --noedit --needed --noconfirm )
do_upgrade || TRIZEN_SYNC_OPTS=( "${TRIZEN_SYNC_OPTS[@]}" --nopull )

trizen_pkgs=()

trizen_sync_pkg() {
    local pkg="$1"

    trizen_pkgs+=( "$pkg" )
}


trizen_reduce_pkgs() {
    hash trizen 2>/dev/null || die 'trizen is not found.'

    progress 'Installing trizen packages ...'

    # If there're no packages to install, do nothing
    if [[ "${#trizen_pkgs}" == 0 ]]; then
        warn "There're no trizen packages to install."
        return
    fi

    facade_exec_cmd trizen "${TRIZEN_SYNC_OPTS[@]}" "${trizen_pkgs[@]}"
}

facade_add_reducer trizen_reduce_pkgs
