# func_defs_emacs.zsh
# author: Seong Yong-ju <sei40kr@gmail.com>

_emacsfun() {
    if displays_graphic; then
        emacsclient -ca '' \
                    -s ~/.emacs.d/server/server \
                    "$@"
    else
        env TERM=xterm-256color emacsclient \
            -tca 'emacs -nw' \
            -s ~/.emacs.d/server-tui/server \
            "$@"
    fi
}
