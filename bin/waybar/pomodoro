#!/usr/bin/env bash

elapsed="$(dconf read '/org/gnome/pomodoro/state/timer-elapsed')"
paused="$(dconf read '/org/gnome/pomodoro/state/timer-paused')"
state="$(dconf read '/org/gnome/pomodoro/state/timer-state')"
state="${state#\'}"
state="${state%\'}"
state_duration="$(dconf read '/org/gnome/pomodoro/state/timer-state-duration')"

{
  echo "{\"Elapsed\":${elapsed},\"IsPaused\":${paused},\"State\":\"${state}\",\"StateDuration\":${state_duration}}"
  busctl --user --json=short monitor org.gnome.Pomodoro 2>/dev/null |
    jq -cM --unbuffered 'select(.member == "PropertiesChanged") | .payload.data[1] | .[] |= .data'
} | jq -ncM --unbuffered 'foreach inputs as $item (
    {}; . + $item; {
      text: (if .State != "null" then (.StateDuration - .Elapsed | strftime("%M:%S")) else "" end),
      alt: (if .IsPaused then "paused" else .State end),
      class: (if .IsPaused then "paused" else .State end)
    }
  )'
