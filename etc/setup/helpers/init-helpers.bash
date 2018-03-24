#!/usr/bin/env bash

# init-helpers.bash - Initializes all helpers
# author: Seong Yong-ju <sei40kr@gmail.com>

_init_helpers() {
  local current_dir
  current_dir="$(dirname "${BASH_SOURCE[0]}")"

  # shellcheck source=b-log.sh
  . "${current_dir}/b-log.sh"

  _init_b_log
}

_init_b_log() {
  B_LOG -l $LOG_LEVEL_DEBUG
}

_init_helpers
