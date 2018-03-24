#!/usr/bin/env bash

# 400_version-manager-anyenv.bash - anyenv installer
# author: Seong Yong-ju <sei40kr@gmail.com>

set -euo pipefail
IFS=$'\n\t'

# shellcheck source=helpers/init-helpers.bash
. "$(dirname "${BASH_SOURCE[0]}")/helpers/init-helpers.bash"


TRACE 'Checking anyenv is installed.'
if [[ ! -d "${HOME}/.anyenv" ]]; then
  INFO 'Installing anyenv.'
  git clone --depth=1 https://github.com/riywo/anyenv "${HOME}/.anyenv" | DEBUG
fi
