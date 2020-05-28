# custom-history.zsh
# author: Seong Yong-ju <sei40kr@gmail.com>

typeset -g custom_history__last_line

custom_history__zshaddhistory() {
    custom_history__last_line="${1%%$'\n'}"
    return 2
}

zshaddhistory_functions+=( custom_history__zshaddhistory )

custom_history__precmd() {
    local exit_code="$?"
    local line="${custom_history__last_line}"
    custom_history__last_line=''

    if [[ "$exit_code" != 0 && "$exit_code" != 130 ]]; then
        return
    fi

    # HIST_IGNORE_SPACE
    if [[ "$line" = ' '* ]]; then
        return
    fi

    # HIST_REDUCE_BLANKS
    line="${(z)line}"

    print -sr -- "$line"
}

precmd_functions+=( custom_history__precmd )
