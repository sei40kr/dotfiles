# func_defs_emacs.zsh
# author: Seong Yong-ju <sei40kr@gmail.com>

_emacsfun() {
    if displays_graphic; then
        emacsclient -ca '' "$@"
    else
        env TERM=xterm-256color emacsclient -tca '' "$@"
    fi
}
