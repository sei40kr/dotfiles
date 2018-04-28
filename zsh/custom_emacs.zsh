# custom_emacs.zsh
# author: Seong Yong-ju <sei40kr@gmail.com>

if [[ "$OSTYPE" = linux* ]]; then
  export XLIB_SKIP_ARGB_VISUALS=1
fi

e() {
  if [[ "$2" = '-' ]]; then
    tempfile="$(mktemp "tmp.emacs.XXXXXXX" --tmpdir)"
    cat - >"$tempfile"
    emacsclient -a '' -c "$tempfile"
  else
    emacsclient -a '' -e '(frame-list)' 2>/dev/null \
      | grep -oE '#<frame\s+[+>]+>' \
      && emacsclient -a '' "$@" \
      || emacsclient -a '' -c "$@"
  fi
}

alias emacs='e -n'
alias te='e -nw'

alias eframe="emacsclient -a '' -c"

alias ekill="emacsclient -e '(kill-emacs)'"

efile() {
  emacsclient -e '(buffer-file-name (window-buffer))' | tr -d '"'
}
