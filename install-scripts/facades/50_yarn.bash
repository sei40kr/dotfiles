# 50_yarn.bash --- yarn facade
# author: Seong Yong-ju <sei40kr@gmail.com>

__yarn_pkgs=()

yarn_global_add_facade() {
    local pkg="$1"

    __yarn_pkgs+=( "$pkg" )
}


install_yarn_or_err() {
    if ! is_dry_run; then
        if is_arch; then
            sudo pacman -S \
                 --needed \
                 --noconfirm \
                 --noprogressbar \
                 --assume-installed nodejs \
                 yarn
            return
        fi

        die 'yarn is not found.'
    fi
}

yarn_global_add_facade_reducer() {
    [[ "${#__yarn_pkgs[@]}" == 0 ]] && return

    ! hash yarn 2>/dev/null && install_yarn_or_err

    log_wait 'Installing Yarn packages ...'

    if do_update; then
        wrap_facade_cmd yarn global add \
                        -s \
                        --noprogress \
                        --non-interactive \
                        --latest \
                        "${__yarn_pkgs[@]}"
    else
        wrap_facade_cmd yarn global add \
                        -s \
                        --noprogress \
                        --non-interactive \
                        "${__yarn_pkgs[@]}"
    fi
}

register_facade_reducer yarn_global_add_facade_reducer
