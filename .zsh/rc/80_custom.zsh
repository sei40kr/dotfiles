#!/usr/bin/env zsh

# 80_custom.zsh
# author: Seong Yong-ju ( @sei40kr )

# robbyrussell/oh-my-zsh/colored-man-pages {{{
man() {
  command env \
    LESS_TERMCAP_mb=$(printf "\e[1;31m") \
    LESS_TERMCAP_md=$(printf "\e[1;31m") \
    LESS_TERMCAP_me=$(printf "\e[0m") \
    LESS_TERMCAP_se=$(printf "\e[0m") \
    LESS_TERMCAP_so=$(printf "\e[1;44;33m") \
    LESS_TERMCAP_ue=$(printf "\e[0m") \
    LESS_TERMCAP_us=$(printf "\e[1;32m") \
    _NROFF_U=1 \
    man "$@"
}
# }}}

# mollifier/anyframe {{{
anyframe-widget-cd-ghq-repository() {
  anyframe-source-ghq-repository | \
    anyframe-selector-auto | \
    anyframe-action-execute builtin cd --
}
# }}}
