# custom-history.zsh
# author: Seong Yong-ju <sei40kr@gmail.com>

typeset -g __history_last_cmd

__remember_history_line() {
    __history_line="${1%%$'\n'}"
    return 1
}

zshaddhistory_functions+=( __remember_history_line )

__save_history_line() {
    local exit_code="$?"
    local history_line="${__history_line}"

    if [[ "$exit_code" != 0 ]]; then
        return
    fi

    # HIST_IGNORE_SPACE
    if [[ "$__history_line" = ' '* ]]; then
        return
    fi

    # HIST_REDUCE_BLANKS
    history_line="${(z)history_line}"

    print -sr -- "$history_line"
}

precmd_functions+=( __save_history_line )
