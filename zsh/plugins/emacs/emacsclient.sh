#!/bin/sh

# cf https://github.com/robbyrussell/oh-my-zsh/blob/master/plugins/emacs/emacsclient.sh

_emacsfun() {
  # get list of emacs frames.
  frameslist="$(emacsclient -a '' -e '(frame-list)' 2>/dev/null | grep -oE '(frame)+')"

  if [ "$(echo "$frameslist" | sed -n '$=')" -ge 2 ]; then
    # prevent creating another X frame if there is at least one present.
    emacsclient -a "" "$@"
  else
    # Create one if there is no X window yet.
    emacsclient -a "" -c "$@"
  fi
}


# adopted from https://github.com/davidshepherd7/emacs-read-stdin/blob/master/emacs-read-stdin.sh
# If the second argument is - then write stdin to a tempfile and open the
# tempfile. (first argument will be `--no-wait` passed in by the plugin.zsh)
if [ "$#" -ge 2 ] && [ "$2" = '-' ]; then
  tempfile="$(mktemp "emacs-stdin-${USER}.XXXXXXX" --tmpdir)"
  cat - >"$tempfile"
  _emacsfun -n "$tempfile"
else
  _emacsfun "$@"
fi
