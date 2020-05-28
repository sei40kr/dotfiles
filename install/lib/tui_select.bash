# author: Seong Yong-ju <sei40kr@gmail.com>

shopt -s extglob

declare -a tui_select__option_labels
declare -a tui_select__option_callbacks
declare tui_select__quit_option_letter
declare tui_select__quit_option_label

# tui_add_options (LABEL CALLBACK) ...
#
tui_add_options() {
  local -a args=("$@")
  assert_equal "$(($# % 2))" 0

  local label
  for ((i = 0; i < $#; i += 2)); do
    label="${args[$i]}"
    assert_not_empty "$label"
    tui_select__option_labels+=("$label")
  done

  local callback
  for ((i = 1; i < $#; i += 2)); do
    callback="${args[$i]}"
    assert_not_empty "$callback"
    tui_select__option_callbacks+=("${args[$i]}")
  done
}

# tui_set_quit_option LETTER LABEL
#
tui_set_quit_option() {
  local letter="$1"
  local label="$2"
  assert_not_empty "$letter"
  assert_not_empty "$label"

  tui_select__quit_option_letter="$letter"
  tui_select__quit_option_label="$label"
}

# tui_select_option PROMPT
#
tui_select_option() {
  local prompt="$1"
  local options_count="${#tui_select__option_labels[@]}"

  tui_select__print_options

  local input
  local callback
  while true; do
    tui_select__print_prompt "$prompt"
    read -r input
    input="$(tui_select__trim_input "$input")"

    if [[ "$input" == +([0-9]) && "$input" -ge 1 && "$input" -le "$options_count" ]]; then
      local i="$(($input - 1))"
      local callback="${tui_select__option_callbacks[$i]}"

      tui_select__clear_options

      eval "$callback"
      return
    elif [[ "$input" == "$tui_select__quit_option_letter" ]]; then
      tui_select__clear_options
      return 1
    elif [[ "$input" != '' ]]; then
      echo "\"${input}\" is invalid." >&2
    fi
  done
}

# tui_select__print_options
#
tui_select__print_options() {
  local options_count="${#tui_select__option_labels[@]}"

  local label
  for ((i = 0; i < $options_count; i++)); do
    label="${tui_select__option_labels[$i]}"
    printf "%${#options_count}d) %s\n" "$(($i + 1))" "$label"
  done

  if [[ -n "$tui_select__quit_option_letter" ]]; then
    echo ''
    printf "%${#options_count}s) %s\n" "${tui_select__quit_option_letter}" \
      "${tui_select__quit_option_label}"
  fi

  echo ''
}

# tui_select__print_prompt
#
tui_select__print_prompt() {
  local prompt="$1"
  local options_count="${#tui_select__option_labels[@]}"

  local input_patterns
  if [[ "$options_count" -gt 0 ]]; then
    input_patterns='1'
  fi
  if [[ "$options_count" -gt 1 ]]; then
    input_patterns="${input_patterns}-${options_count}"
  fi
  if [[ -n "$tui_select__quit_option_letter" ]]; then
    input_patterns="${input_patterns:+${input_patterns},}${tui_select__quit_option_letter}"
  fi

  echo -n "${prompt} (${input_patterns}): "
}

# tui_select__trim_input INPUT
#
tui_select__trim_input() {
  local input="$1"

  input="${input##+( )}"
  input="${input%%+( )}"

  echo "$input"
}

# tui_select__clear_options
#
tui_select__clear_options() {
  tui_select__option_labels=()
  tui_select__option_callbacks=()
  tui_select__quit_option_letter=''
  tui_select__quit_option_label=''
}
