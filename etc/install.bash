#!/usr/bin/env bash

# install.bash
# Seong Yong-ju <sei40kr@gmail.com>

set -euo pipefail
IFS=$'\n\t'

# shellcheck source=setup/helpers/init-helpers.bash
. "$(dirname "${BASH_SOURCE[0]}")/setup/helpers/init-helpers.bash"


current_dir="$(dirname "${BASH_SOURCE[0]}")"

find "${current_dir}/setup" \
     -mindepth 1 \
     -maxdepth 1 \
     -type f \
     -name '*.bash' \
  | sort \
  | while read -r script_path; do
  if [[ -x "$script_path" ]]; then
    bash "${script_path}"
  fi
done
