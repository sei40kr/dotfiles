#!/usr/bin/env bash

# pacman.bash --- pacman facade
# author: Seong Yong-ju <sei40kr@gmail.com>

PACMAN_SYNC_OPTS=( -S --needed --noconfirm --noprogressbar )

pacman_pkgs=()

pacman_sync_pkg() {
    pacman_pkgs=( "${pacman_pkgs[@]}" "$1" )
}


## Reducer

pacman_reduce_pkgs() {
    if ! hash pacman 2>/dev/null; then
        echo 'ERROR: pacman was not found or is not executable.' >&2
        exit 1
    fi

    # If there're no packages to install, do nothing
    if [[ "${#pacman_pkgs}" == 0 ]]; then
        return
    fi

    facade_exec_cmd sudo pacman "${PACMAN_SYNC_OPTS[@]}" "${pacman_pkgs[@]}"
}

facade_add_reducer pacman_reduce_pkgs
