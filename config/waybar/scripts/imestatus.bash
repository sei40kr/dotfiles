#!/usr/bin/env bash

case $(fcitx5-remote) in
  0 | 1)
    echo 'Aa'
    ;;
  2)
    echo 'あ'
    ;;
esac
