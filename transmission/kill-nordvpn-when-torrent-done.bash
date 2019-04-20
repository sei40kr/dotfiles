#!/usr/bin/env bash

# kill-nordvpn-when-torrent-done
# author: Seong Yong-ju <sei40kr@gmail.com>

if [[ "$OSTYPE" == darwin* ]]; then
    pkill -HUP NordVPN
fi
