# Emacs
# cf https://github.com/robbyrussell/oh-my-zsh/blob/master/plugins/emacs/emacs.plugin.zsh
efile() {
  local cmd='(buffer-file-name (window-buffer))'
  "$EMACS_PLUGIN_LAUNCHER" --eval "$cmd" | tr -d '"'
}

ecd() {
  local cmd='
(let ((buf-name (buffer-file-name (window-buffer))))
  (if buf-name
      (file-name-directory buf-name)))'
  local dir="$($EMACS_PLUGIN_LAUNCHER --eval $cmd | tr -d \")"

  if [[ -n "$dir" ]]; then
    echo "$dir"
  else
    echo "can not deduce current buffer filename." >&2
    return 1
  fi
}
