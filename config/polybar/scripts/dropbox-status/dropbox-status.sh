#!/bin/sh

# dropbox-status.sh
# author: Seong Yong-ju <sei40kr@gmail.com>

basedir="$(dirname "$0")"

status="$("${basedir}/dropbox.py" status)"
case "$status" in
    'Up to date' )
        echo ''
        ;;
    "Couldn't get status:"*|"Dropbox isn't responding!" )
        echo ''
        ;;

    'Dropbox daemon stopped.' )
        ;;

    # Syncing
    * )
        echo ''
        ;;
esac
