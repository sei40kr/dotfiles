# custom-history.zsh
# author: Seong Yong-ju <sei40kr@gmail.com>

typeset -g __history_line

__custom_history_zshaddhistory() {
    __history_line="${1%%$'\n'}"
    return 2
}

zshaddhistory_functions+=( __custom_history_zshaddhistory )

__custom_history_precmd() {
    local exit_code="$?"
    local history_line="${__history_line}"
    __history_line=''

    if [[ "$exit_code" != 0 && "$exit_code" != 130 ]]; then
        return
    fi

    # HIST_IGNORE_SPACE
    if [[ "$history_line" = ' '* ]]; then
        return
    fi

    # HIST_REDUCE_BLANKS
    history_line="${(z)history_line}"

    print -sr -- "$history_line"
}

precmd_functions+=( __custom_history_precmd )
