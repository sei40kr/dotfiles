#!/bin/sh

# dropbox-status.sh
# author: Seong Yong-ju <sei40kr@gmail.com>

RED='#ff6c6b'
BLUE='#51afef'

basedir="$(dirname "$0")"

status="$("${basedir}/dropbox.py" status)"
case "$status" in
    'Up to date' )
        echo "%{F${BLUE}}%{T3}%{T-}%{O8}up-to-date%{F-}"
        ;;
    "Couldn't get status:"*|"Dropbox isn't responding!" )
        echo "%{F${RED}}%{T3}%{T-}%{O8}error%{F-}"
        ;;

    'Dropbox daemon stopped.' )
        ;;

    # Syncing
    * )
        echo "%{F${BLUE}}%{T3}%{T-}%{O8}syncing%{F-}"
        ;;
esac
