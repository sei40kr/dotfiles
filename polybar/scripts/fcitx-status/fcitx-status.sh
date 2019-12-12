#!/bin/sh

# fcitx-status.sh
# author: Seong Yong-ju <sei40kr@gmail.com>

dbus-monitor --profile path=/inputmethod,interface=org.freedesktop.DBus.Properties,member=PropertiesChanged |
    while read -r line; do
    if [[ "$(fcitx-remote status)" == 2 ]]; then
        echo '%{O3}あ%{O2}'
    else
        echo 'Aa'
    fi
done
