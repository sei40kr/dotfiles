# tui.bash
# author: Seong Yong-ju

COLUMNS="$(tput cols)"

BOLD="$(tput bold)"
RED="$(tput setaf 1)"
GREEN="$(tput setaf 2)"
YELLOW="$(tput setaf 3)"
BLUE="$(tput setaf 4)"
PURPLE="$(tput setaf 5)"
CYAN="$(tput setaf 6)"
WHITE="$(tput setaf 7)"
RESET="$(tput sgr0)"

print_line() {
  local empty_line="$(printf "%${COLUMNS}s")"

  echo "${empty_line// /-}"
}

print_title() {
  local title="$1"

  clear
  print_line
  echo "# ${BOLD}${title}${RESET}"
  print_line
  echo ''
}

print_desc() {
  local desc="$1"

  echo "$desc"
  echo ''
}

menu_item() {
  local title="$1"

  echo "${BOLD}${title}${RESET}"
}

print-list-item() {
  local title="$1"

  tui-print "- ${title}"
}

print-step() {
  local step="$1"

  tui-print "→ ${step}"
}

ask-yesno() {
  local prompt="$1"

  tui-print-prompt "${prompt} [yn]"

  while true; do
    read -n1 -r answer

    echo
    if [[ "$answer" == [yY]* ]]; then
      return 0
    elif [[ "$answer" == [nN]* ]]; then
      return 1
    else
      tui-print-prompt "Invalid answer. ${prompt} [yn]"
    fi
  done
}

pause() {
  print_line
  read -sn 1 -p 'Press enter to continue ...'
}
