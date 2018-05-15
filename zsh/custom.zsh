# custom.zsh
# author: Seong Yong-ju <sei40kr@gmail.com>

setopt APPEND_HISTORY \
       AUTO_PARAM_KEYS \
       AUTO_RESUME \
       EQUALS \
       EXTENDED_HISTORY \
       GLOB_DOTS \
       HIST_REDUCE_BLANKS \
       INTERACTIVE_COMMENTS \
       NO_BEEP \
       NUMERIC_GLOB_SORT \
       PRINT_EIGHT_BIT \
       PROMPT_SUBST \
       SHARE_HISTORY
unsetopt LIST_BEEP

alias u='cd ..'
alias reload='. ~/.zsh/.zshrc'

alias ranger='[ -z "$RANGER_LEVEL" ] && \ranger'
