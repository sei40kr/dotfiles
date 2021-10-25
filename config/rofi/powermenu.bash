#!/usr/bin/env bash

list_items() {
  echo -e "Sleep\0icon\x1fsystem-suspend-symbolic\x1finfo\x1fsleep"
  echo -e "Shutdown\0icon\x1fsystem-shutdown-symbolic\x1finfo\x1fshutdown"
  echo -e "Reboot\0icon\x1fsystem-restart-symbolic\x1finfo\x1freboot"
}

case "$ROFI_RETV" in
  # Initial call of script
  0)
    echo -e '\0prompt\x1fPower'
    echo -e '\0no-custom\x1ftrue'
    ;;

  # Selected an entry
  1)
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
  ;;
esac

list_items
