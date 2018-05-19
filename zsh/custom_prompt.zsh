#!/usr/bin/zsh

# custom_prompt.zsh
# author: Seong Yong-ju <sei40kr@gmail.com>

print-toggl-current() {
  toggl --cache --csv current | \
    awk -F',' '
      $1 == "Description" { if (length($2) > 20) print substr($2, 0, 20) "..."; else print $2 }
      $1 == "Duration" { split($2, t, ":"); print t[1] " hrs " t[2] " min" }' ORS=' '
}

ZLE_RPROMPT_INDENT=0
RPROMPT='%F{yellow}$(print-toggl-current)%f'
