#!/usr/bin/env bash

if [[ "$OSTYPE" == darwin* ]]; then
    rcup -t macos
else
    rcup -t linux
fi
