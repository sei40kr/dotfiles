#!/usr/bin/env bash

if [[ "$ROFI_RETV" == 1 ]]; then
  case "$ROFI_INFO" in
    'sleep')
      systemctl suspend
      exit
      ;;
    reboot)
      systemctl reboot
      exit
      ;;
    shutdown)
      systemctl poweroff
      exit
      ;;
  esac
fi

echo -e '\0no-custom\x1ftrue'
echo -e "System: Sleep\0info\x1fsleep"
echo -e "System: Shutdown\0info\x1fshutdown"
echo -e "System: Reboot\0info\x1freboot"
