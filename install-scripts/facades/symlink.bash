# symlink.bash --- A facade for a symbolic link
# author: Seong Yong-ju <sei40kr@gmail.com>

SYMLINK_SRC_BASEDIR="${BASEDIR}/install-files"
SYMLINK_DEST_BASEDIR="${HOME}"

SYMLINK_LN_OPTS=( -fLsT )
[[ "$verbose" == 1 ]] && SYMLINK_LN_OPTS="${SYMLINK_LN_OPTS}v" || :

symlink_srcs=()
symlink_dests=()

make_symlink() {
    [[ "${1:0:1}" != '/' ]] && local src="${SYMLINK_SRC_BASEDIR}/${1}" || local src="$1"
    [[ "${2:0:1}" != '/' ]] && local dest="${SYMLINK_DEST_BASEDIR}/${2}" || local dest="$2"
    symlink_srcs=( "${symlink_srcs[@]}" "$src" )
    symlink_dests=( "${symlink_dests[@]}" "$dest" )
}


## Reducer

symlink_reduce_symlinks() {
    for i in "${!symlink_srcs[@]}"; do
        local src="${symlink_srcs[$i]}"
        local dest="${symlink_dests[$i]}"

        local basedir="$(basename "$dest")"
        mkdir -p "$basedir"

        ln "${SYMLINK_LN_OPTS[@]}" "$src" "$dest"
    done
}

add_facade_reducer symlink_reduce_symlinks
