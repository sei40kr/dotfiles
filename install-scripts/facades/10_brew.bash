# 10_brew.bash --- brew facade
# author: Seong Yong-ju <sei40kr@gmail.com>

__brew_repos=()
__brew_repo_urls=()

__brew_formulas=()

__brew_casks=()

brew_tap_facade() {
    local repo="$1"
    local url="$2"

    __brew_repos+=( "$repo" )
    __brew_urls+=( "$url" )
}

brew_install_facade() {
    local formula="$1"

    __brew_formulas+=( "$formula" )
}

brew_cask_install_facade() {
    local cask="$1"

    __brew_casks+=( "$cask" )
}


__dump_brewfile() {
    for i in "${!__brew_repos[@]}"; do
        local repo="${__brew_repos[$i]}"
        local url="${__brew_repo_urls[$i]}"

        if [[ "$url" == '' ]]; then
            printf 'tap "%s"\n' "$repo"
        else
            printf 'tap "%s", "%s"\n' "$repo" "$url"
        fi
    done

    printf 'brew "%s"\n' "${__brew_formulas[@]}"

    if [[ "${#__brew_casks}" != 0 ]]; then
        echo 'cask_args appdir: "~/Applications"'
        printf 'cask "%s"\n' "${__brew_casks[@]}"
    fi
}


__BREW_BUNDLE_CMD=( brew bundle --file=- )

brew_facade_reducer() {
    [[ "${#__brew_repos}" == 0 ]] &&
        [[ "${#__brew_formulas}" == 0 ]] &&
        [[ "${#__brew_casks}" == 0 ]] &&
        return

    # TODO Install Homebrew/Linuxbrew
    ! hash brew 2>/dev/null && die 'brew is not found.'

    log_wait 'Installing Homebrew formulas, casks ...'

    if is_dry_run; then
        cat <<CMD
> ${__BREW_BUNDLE_CMD[@]} <<BREWFILE
$(__dump_brewfile | awk '{ print "> " $0 }')
> BREWFILE
CMD
    else
        __dump_brewfile | wrap_facade_cmd "${__BREW_BUNDLE_CMD[@]}"
    fi
}

register_facade_reducer brew_facade_reducer
