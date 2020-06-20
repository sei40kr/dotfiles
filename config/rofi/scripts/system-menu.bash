#!/usr/bin/env bash
# author: Seong Yong-ju <sei40kr@gmail.com>

items=(
    Lock 'xautolock -locknow'
    Suspend 'systemctl suspend -i'
    Reboot 'systemctl reboot -i'
    Shutdown 'systemctl poweroff -i'

    'Clear Clipboard History' "clipdel -d '.*'"
)

selection="$1"

if [[ -z "$selection" ]]; then
    for (( i = 0; i < ${#items[@]}; i+=2 )); do
        echo "${items[$i]}"
    done
else
    for (( i = 0; i < ${#items[@]}; i+=2 )); do
        if [[ "${items[$i]}" == "$selection" ]]; then
            break
        fi
    done

    eval "${items[$((++i))]}"
fi
