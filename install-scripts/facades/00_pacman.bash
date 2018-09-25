# 00_pacman.bash --- pacman facade
# author: Seong Yong-ju <sei40kr@gmail.com>

__pacman_pkgs=()

pacman_sync_facade() {
    local pkg="$1"

    __pacman_pkgs+=( "$pkg" )
}


pacman_sync_facade_reducer() {
    [[ "${#__pacman_pkgs[@]}" == 0 ]] && return

    ! is_arch && die 'pacman can be used only on Arch Linux.'
    ! hash pacman 2>/dev/null && die 'pacman is not found.'

    log_wait 'Installing pacman packages ...'

    wrap_facade_cmd sudo pacman -S --needed --noconfirm --noprogressbar "${__pacman_pkgs[@]}"
}

register_facade_reducer pacman_sync_facade_reducer
