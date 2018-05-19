#!/usr/bin/zsh

# custom_prompt.zsh
# author: Seong Yong-ju <sei40kr@gmail.com>

print-toggl-current() {
  toggl --cache --csv current | \
    awk -F',' '
      $1 == "Description" { if (length($2) > 20) print substr($2, 0, 20) "..."; else print $2 }
      $1 == "Duration" { print $2 }' ORS=' '
}

ZLE_RPROMPT_INDENT=0
RPROMPT='%F{yellow}$(print-toggl-current)%f'
