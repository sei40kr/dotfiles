#!/usr/bin/env bash

if [[ -z "$SWAYSOCK" ]]; then
  exit
fi

if [[ "$ROFI_RETV" == 1 ]]; then
  swaymsg "[con_id=${ROFI_INFO}] focus" >/dev/null
  exit
fi

echo -e '\0prompt\x1fWindows'
echo -e '\0no-custom\x1ftrue'
swaymsg -t get_tree | jq -r 'recurse(.nodes[]) | select(.app_id != null or .window != null) | "\(.name)\u0000info\u001f\(.id)"'
