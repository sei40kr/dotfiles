# custom-history.zsh
# author: Seong Yong-ju <sei40kr@gmail.com>

typeset -g __history_last_cmd

__remember_last_cmd() {
    __history_last_cmd="${1%%$'\n'}"
    return 1
}

zshaddhistory_functions+=( __remember_last_cmd )

__append_history() {
    local last_exit_code="$?"
    local last_cmd="${__history_last_cmd}"

    if [[ "$last_exit_code" != 0 ]]; then
        return
    fi

    # HIST_IGNORE_SPACE
    if [[ "$__history_last_cmd" = ' '* ]]; then
        return
    fi

    # HIST_REDUCE_BLANKS
    last_cmd="${(z)last_cmd}"

    print -sr -- "$last_cmd"
}

precmd_functions+=( __append_history )
