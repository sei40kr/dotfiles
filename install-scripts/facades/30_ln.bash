# ln.bash --- ln facade
# author: Seong Yong-ju <sei40kr@gmail.com>

__ln_srcs=()
__ln_dests=()

ln_facade() {
    local src="$1"
    local dest="$2"

    __ln_srcs+=( "$src" )
    __ln_dests+=( "$dest" )
}


ln_facade_reducer() {
    [[ "${#__ln_srcs[@]}" == 0 ]] && return

    log_wait 'Creating symlinks ...'

    for i in "${!__ln_srcs[@]}"; do
        local src="${__ln_srcs[$i]}"
        local dest="${__ln_dests[$i]}"

        wrap_facade_cmd mkdir -p "$(dirname "$dest")"
        wrap_facade_cmd ln -fLsT "$src" "$dest"
    done
}

register_facade_reducer ln_facade_reducer
