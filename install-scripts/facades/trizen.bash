# trizen.bash --- trizen facade
# author: Seong Yong-ju <sei40kr@gmail.com>

TRIZEN_SYNC_OPTS=( -S --noedit --needed --noconfirm )
[[ "$upgrade" != 1 ]] && TRIZEN_SYNC_OPTS=( "${TRIZEN_SYNC_OPTS[@]}" --nopull ) || :

trizen_pkgs=()

trizen_sync_pkg() {
    trizen_pkgs=( "${trizen_pkgs[@]}" "$1" )
}


## Reducer

trizen_reduce_pkgs() {
    if ! hash trizen 2>/dev/null; then
        echo 'ERROR: trizen was not found or is not executable.' >&2
        exit 1
    fi

    # If there're no packages to install, do nothing
    if [[ "${#trizen_pkgs}" == 0 ]]; then
        return
    fi

    trizen "${TRIZEN_SYNC_OPTS[@]}" "${trizen_pkgs[@]}"
}

add_facade_reducer trizen_reduce_pkgs
