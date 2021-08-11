#!/usr/bin/env bash

list_menus() {
  echo -e "Sleep\0info\x1fsleep"
  echo -e "Reboot\0info\x1freboot"
  echo -e "Shutdown\0info\x1fshutdown"
}

do_action() {
  case $ROFI_INFO in
    'sleep')
      systemctl suspend
      ;;
    reboot)
      systemctl reboot
      ;;
    shutdown)
      systemctl poweroff
      ;;
  esac
}

case $ROFI_RETV in
  0)
    list_menus
    ;;
  1)
    do_action
    ;;
esac
