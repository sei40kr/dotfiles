# module-utility.bash --- Utility functions for modules
# author: Seong Yong-ju <sei40kr@gmail.com>

declare -A __sourced_facades

. "${BASEDIR}/install-scripts/facade-utility.bash"

require_facades() {
    local facades=( "$@" )

    for facade in "${facades[@]}"; do
        if [[ "${__sourced_facades[$facade]}" != 1 ]]; then
            local srcpath="${BASEDIR}/install-scripts/facades/${facade}.bash"

            [[ ! -s "$srcpath" ]] && die "Facade ${facade} is not found"

            . "$srcpath"
            __sourced_facades[$facade]=1
        fi
    done
}
