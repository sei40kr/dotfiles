# author: Seong Yong-ju <sei40kr@gmail.com>

shopt -s extglob

declare -a __tui_select_option_labels
declare -a __tui_select_option_callbacks
declare __tui_select_quit_option_letter
declare __tui_select_quit_option_label

# tui_init_options
#
# TODO
#
tui_init_options() {
  __tui_select_option_labels=()
  __tui_select_option_callbacks=()
  __tui_select_quit_option_letter=''
  __tui_select_quit_option_label=''
}

# tui_add_options (LABEL CALLBACK) ...
#
# TODO
#
tui_add_options() {
  local -a args=("$@")
  local num_args="${#args[@]}"
  assert_equal "$(($num_args % 2))" 0

  local label
  for ((i = 0; i < $num_args; i += 2)); do
    label="${args[$i]}"
    assert_not_empty "$label"
    __tui_select_option_labels+=("$label")
  done

  local callback
  for ((i = 1; i < $num_args; i += 2)); do
    callback="${args[$i]}"
    assert_not_empty "$callback"
    __tui_select_option_callbacks+=("${args[$i]}")
  done
}

# tui_set_quit_option LETTER LABEL
#
# TODO
#
tui_set_quit_option() {
  local letter="$1"
  local label="$2"
  assert_not_empty "$letter"
  assert_not_empty "$label"

  __tui_select_quit_option_letter="$letter"
  __tui_select_quit_option_label="$label"
}

# tui_select_option PROMPT
#
# TODO
#
tui_select_option() {
  local prompt="$1"

  local num_options="${#__tui_select_option_labels[@]}"

  local label
  for ((i = 0; i < $num_options; i++)); do
    label="${__tui_select_option_labels[$i]}"
    printf "%${#num_options}d) %s\n" "$(($i + 1))" "$label"
  done
  if [[ -n "$__tui_select_quit_option_letter" ]]; then
    echo ''
    printf "%${#num_options}s) %s\n" "${__tui_select_quit_option_letter}" \
      "${__tui_select_quit_option_label}"
  fi
  echo ''

  local pattern='1'
  if [[ "$num_options" -gt 1 ]]; then
    pattern="${pattern}-${num_options}"
  fi
  if [[ -n "$__tui_select_quit_option_letter" ]]; then
    pattern="${pattern},${__tui_select_quit_option_letter}"
  fi

  while true; do
    read -r -p "${prompt} (${pattern}): " input
    input="${input##+( )}"
    input="${input%%+( )}"

    if [[ "$input" == +([0-9]) && "$input" -ge 1 && "$input" -le "$num_options" ]]; then
      local i="$(($input - 1))"
      eval "${__tui_select_option_callbacks[$i]}"
      break
    elif [[ "$input" == "$__tui_select_quit_option_letter" ]]; then
      return 1
    else
      echo "\"${input}\" is invalid." >&2
    fi
  done
}
