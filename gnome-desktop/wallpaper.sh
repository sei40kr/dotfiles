#!/usr/bin/env bash

# wallpaper.sh
# author: Seong Yong-ju <sei40kr@gmail.com>

set -euo pipefail
IFS=$'\n\t'

while true; do
    find /usr/share/wallpapers/chromecast1200 \
         -mindepth 1 \
         -maxdepth 1 \
         -name '*.jpg' \
         -type f |
        shuf |
        while read -r picture_filepath; do
            dconf write /org/gnome/desktop/background/picture-uri \
                  "'file://${picture_filepath}'"

            sleep 3m
        done
done
