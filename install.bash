#!/usr/bin/env bash

set -euo pipefail
IFS=$'\n\t'

if [[ "$OSTYPE" == darwin* ]]; then
    rcup -t macos
else
    rcup -t linux
fi
