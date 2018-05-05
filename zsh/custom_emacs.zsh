# custom_emacs.zsh
# author: Seong Yong-ju <sei40kr@gmail.com>

if [[ "$OSTYPE" = linux* ]]; then
  alias e='XLIB_SKIP_ARGB_VISUALS=1 e'
fi

alias emacs='e -n'
alias te='e -nw'

alias eframe="emacsclient -a '' -c"
alias ekill="emacsclient -e '(kill-emacs)'"

alias efile="emacsclient -e '(buffer-file-name (window-buffer))' | tr -d '\"'"
