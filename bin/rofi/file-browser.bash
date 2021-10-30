#!/usr/bin/env bash

shopt -s extglob

if [[ "$ROFI_RETV" == 1 && ! -d "$ROFI_INFO" ]]; then
  xdg-open "$ROFI_INFO"
  exit
fi

cwd="${ROFI_INFO:-${HOME}}"
if [[ "$cwd" == '~'* ]]; then
  cwd="${cwd/~/${HOME}}"
fi

echo -e '\0prompt\x1fFiles'
echo -e "\0message\x1f${cwd}"

if [[ "$cwd" != '/' ]]; then
  echo -e "..\0icon\x1fgo-parent-folder\x1finfo\x1f${cwd%/*}"
fi

fd -uu -a --exact-depth 1 -t d -x echo -e '{/}\0icon\x1ffolder-symbolic\x1finfo\x1f{}'
fd -uu -a --exact-depth 1 -t f -t l -x echo -e '{/}\0info\x1f{}'
