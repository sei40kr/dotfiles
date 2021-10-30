#!/usr/bin/env bash

if [[ "$ROFI_RETV" == 1 ]]; then
  cmd="$ROFI_INFO"
  playerctl -s "$cmd"
  exit
fi

case "$(playerctl -s status)" in
  Stopped)
    exit
    ;;
  Playing)
    echo -e 'Media: Pause\0info\x1fpause'
    ;;
  Paused)
    echo -e 'Media: Play\0info\x1fplay'
    ;;
esac

echo -e 'Media: Stop\0info\x1fstop'
