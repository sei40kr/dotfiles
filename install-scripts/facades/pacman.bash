# pacman.bash --- pacman facade
# author: Seong Yong-ju <sei40kr@gmail.com>

PACMAN_SYNC_OPTS=( -S --needed --noconfirm --noprogressbar )

pacman_pkgs=()

pacman_sync_pkg() {
    local pkg="$1"

    pacman_pkgs+=( "$pkg" )
}


pacman_reduce_pkgs() {
    hash pacman 2>/dev/null || die 'pacman is not found.'

    progress 'Installing pacman packages ...'

    # If there're no packages to install, do nothing
    if [[ "${#pacman_pkgs}" == 0 ]]; then
        warn "There're no pacman packages to install."
        return
    fi

    facade_exec_cmd sudo pacman "${PACMAN_SYNC_OPTS[@]}" "${pacman_pkgs[@]}"
}

facade_add_reducer pacman_reduce_pkgs
