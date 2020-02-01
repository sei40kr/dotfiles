# vterm.zsh
# author: Seong Yong-ju <sei40kr@gmail.com>

alias clear='vterm_printf "51;Evterm-clear-scrollback"; tput clear'

vterm_printf() {
    if [[ -n "$TMUX" ]]; then
        # tell tmux to pass the escape sequences through
        # (Source: http://permalink.gmane.org/gmane.comp.terminal-emulators.tmux.user/1324)
        printf "\ePtmux;\e\e]%s\007\e\\" "$1"
    elif [[ "$TERM" == screen-* ]]; then
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$1"
    else
        printf "\e]%s\e\\" "$1"
    fi
}