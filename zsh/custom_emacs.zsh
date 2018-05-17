# custom_emacs.zsh
# author: Seong Yong-ju <sei40kr@gmail.com>

if [[ "$OSTYPE" = linux* ]]; then
  alias e='XLIB_SKIP_ARGB_VISUALS=1 e'
  export EDITOR='XLIB_SKIP_ARGB_VISUALS=1 e -nw'
fi

alias e='TERM=xterm-256color \e'
alias emacs='e -n'
alias te='e -nw'

alias eframe="TERM=xterm-256color emacsclient -a '' -c"
alias ekill="emacsclient -e '(kill-emacs)'"

alias efile="emacsclient -e '(buffer-file-name (window-buffer))' | tr -d '\"'"
