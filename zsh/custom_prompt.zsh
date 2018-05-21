#!/usr/bin/zsh

# custom_prompt.zsh
# author: Seong Yong-ju <sei40kr@gmail.com>

print_toggl_duration() {
  toggl --cache --csv current | \
    awk -F',' '$1 == "Duration" { split($2, t, ":"); print (t[1] + 0) "h " (t[2] + 0) "m " (t[3] + 0) "s" }'
}

# RPROMPT='%F{yellow}$(print_toggl_duration)%f'

# TMOUT=1
# TRAPALRM() {
#   [[ "$WIDGET" = 'expand-or-complete-with-indicator' ]] && \
#     [[ "$_lastcomp[insert]" =~ "^automenu$|^menu:" ]] || \
#     zle reset-prompt
# }
