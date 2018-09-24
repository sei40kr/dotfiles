# 50_gem.bash --- gem facade
# author: Seong Yong-ju <sei40kr@gmail.com>

__gem_gems=()

gem_install_facade() {
    local pkg="$1"

    __gem_gems+=( "$pkg" )
}

gem_install_facade_reducer() {
    [[ "${#__gem_gems[@]}" == 0 ]] && return

    ! hash gem 2>/dev/null && die 'gem is not found.'

    log_wait 'Installing Rubygems ...'

    if do_update; then
        wrap_facade_cmd gem install -q --silent "${__gem_gems[@]}"
    else
        wrap_facade_cmd gem install --conservative --minimal-deps -q --silent "${__gem_gems[@]}"
    fi
}

register_facade_reducer gem_install_facade_reducer
