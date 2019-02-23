# func_defs.zsh
# author: Seong Yong-ju <sei40kr@gmail.com>

shescape() {
    printf '%q' "$@"
}

displays_graphic() {
    [[ "$SSH_CLIENT" == '' && "$SSH_TTY" == '' ]]
}
