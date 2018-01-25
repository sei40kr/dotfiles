#!/usr/bin/env zsh

# log-helper.zsh
# @author Seong Yong-ju ( @sei40kr )

em_color="${fg[white]}"
cmd_color="${fg[cyan]}"

wait_color="${bg[blue]}${fg[black]}"
done_color="${bg[green]}${fg[black]}"
warn_color="${bg[yellow]}${fg[black]}"
fail_color="${bg[red]}${fg[black]}"


-em() {
  echo "${em_color}${@}${reset_color}"
}

-cmd() {
  echo "${cmd_color}${@}${reset_color}"
}

-log-wait() {
  echo "
${wait_color} wait ${reset_color}" "$@" "
"
}

-log-done() {
  echo "
${done_color} done ${reset_color}" "$@" "
"
}

-log-warn() {
  echo "
${warn_color} warn ${reset_color}" "$@" "
"
}

-log-fail() {
  echo "
${fail_color} fail ${reset_color}" "$@" "
" 1>&2
}
