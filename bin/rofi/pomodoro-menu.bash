#!/usr/bin/env bash

if [[ "$ROFI_RETV" == 1 ]]; then
  opt="$ROFI_INFO"
  gnome-pomodoro "--${opt}"
  exit
fi

state="$(dconf read '/org/gnome/pomodoro/state/timer-state')"
paused="$(dconf read '/org/gnome/pomodoro/state/timer-paused')"

echo -e "\0no-custom\x1ftrue"
if [[ "$state" == "'pomodoro'" ]]; then
  echo -e "Pomodoro: Stop\0info\x1fstop"
  if [[ "$paused" == 'true' ]]; then
    echo -e "Pomodoro: Resume\0info\x1fresume"
  else
    echo -e "Pomodoro: Pause\0info\x1fpause"
  fi
else
  echo -e "Pomodoro: Start\0info\x1fstart"
fi
