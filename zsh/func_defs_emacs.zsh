# func_defs_emacs.zsh
# author: Seong Yong-ju <sei40kr@gmail.com>

_emacsfun() {
    local emacs_path="$(/usr/bin/which emacs)"

    if displays_graphic; then
        emacsclient -ca "$emacs_path" \
                    -s ~/.emacs.d/server/server \
                    "$@"
    else
        env TERM=xterm-256color emacsclient \
            -tca "$emacs_path" \
            -s ~/.emacs.d/server-tui/server \
            "$@"
    fi
}
