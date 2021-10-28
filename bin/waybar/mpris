#!/usr/bin/env bash

playerctl -Fs status | while read -r status; do
  if [[ "$status" != 'Stopped' ]]; then
    playerctl -f '{"text":"{{title}}","alt":"{{lc(status)}}","class":"{{lc(status)}}"}' -s metadata
  fi
done
