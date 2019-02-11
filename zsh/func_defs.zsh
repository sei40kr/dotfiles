# func_defs.zsh
# author: Seong Yong-ju <sei40kr@gmail.com>

# @kawaz's shell string escaping
# https://qiita.com/kawaz/items/f8d68f11d31aa3ea3d1c
shescape() {
    local s
    local bufs=()
    local q="'"
    local qq='"'

    for s in "$@"; do
        bufs+=("'${s//$q/$q$qq$q$qq$q}'")
    done

    echo "${bufs[*]}"
}
