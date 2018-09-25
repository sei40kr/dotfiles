# trizen.bash --- trizen facade
# author: Seong Yong-ju <sei40kr@gmail.com>

__trizen_sync_pkgs=()

trizen_sync_facade() {
    local pkg="$1"

    __trizen_sync_pkgs+=( "$pkg" )
}


trizen_sync_facade_reducer() {
    [[ "${#__trizen_sync_pkgs[@]}" == 0 ]] && return

    if ! hash trizen 2>/dev/null; then
        ! is_arch && die "trizen can be used only on Arch Linux."

        log_wait 'Installing trizen ...'

        # TODO Check whether this installation command works
        git clone --depth 1 https://aur.archlinux.org/trizen.git /tmp/trizen 1>/dev/null &&
            ( cd /tmp/trizen; makepkg -mis --noconfirm --needed ) &&
            rm -rf /tmp/trizen

        # Create a symlink for the Trizen config
        mkdir -p "${XDG_CONFIG_HOME}/trizen"
        ln -fsT "${DOTFILES_PATH}/trizen/trizen.conf" "${XDG_CONFIG_HOME}/trizen/trizen.conf"
    fi

    log_wait 'Installing trizen packages ...'

    if do_update; then
        wrap_facade_cmd trizen -S --noedit --needed --noconfirm "${__trizen_sync_pkgs[@]}"
    else
        wrap_facade_cmd trizen -S --nopull --noedit --needed --noconfirm "${__trizen_sync_pkgs[@]}"
    fi
}

register_facade_reducer trizen_sync_facade_reducer
