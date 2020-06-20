#!/usr/bin/env bash
# author: Seong Yong-ju <sei40kr@gmail.com>

CLIPMENU_MAJOR_VERSION=5
CACHE_DIR="${XDG_RUNTIME_DIR:-${TMPDIR:-/tmp}}/clipmenu.${CLIPMENU_MAJOR_VERSION}.${USER}"

selected_item="$1"

if [[ -z "$selected_item" ]]; then
    LC_ALL=C sort -nrk 1 -u \
          <"${CACHE_DIR}/line_cache_clipboard" \
          <"${CACHE_DIR}/line_cache_primary" |
        cut -d' ' -f2- |
        awk '!seen[$0]++'
else
    file="${CACHE_DIR}/$(cksum <<<"$selected_item")"

    for selection in clipboard primary; do
        xsel -i "--${selection}" <"$file"
    done
fi
