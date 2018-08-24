# arg-parser.bash --- Argument parser for the install script
# author: Seong Yong-ju <sei40kr@gmail.com>

modules_to_install=()

# Create a hash map to check that a module exists efficiently
declare -A __module_map
for module in "${modules[@]}"; do
    __module_map[$module]=1
done

parse_args() {
    for arg in "$@"; do
        case "$arg" in
            --help )
                . "${BASEDIR}/install-scripts/print-help.bash"
                exit
                ;;

            --dry-run )
                dry_run=1
                ;;
            -u|--upgrade )
                upgrade=1
                ;;
            -v|--verbose )
                verbose=1
                ;;

            -*|--* )
                die "Unknown option: ${arg}"
                ;;

            * )
                [[ "${__module_map[$arg]}" != 1 ]] &&
                    die "Module ${arg} is not found"

                modules_to_install+=( "$arg" )
                ;;
        esac
    done

    # If no modules are specified, install all the modules
    if [[ "${#modules_to_install}" == 0 ]]; then
        modules_to_install=( "${modules[@]}" )
    fi
}

do_upgrade() {
    [[ "$upgrade" == 1 ]]
}

is_verbose() {
    [[ "$verbose" == 1 ]]
}

do_dry_run() {
    [[ "$dry_run" == 1 ]]
}
