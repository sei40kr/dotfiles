#!/usr/bin/env bash

# 403_version-manager-anyenv-rbenv.bash - rbenv installer
# author: Seong Yong-ju <sei40kr@gmail.com>

set -euo pipefail
IFS=$'\n\t'

# shellcheck source=helpers/init-helpers.bash
. "$(dirname "${BASH_SOURCE[0]}")/helpers/init-helpers.bash"


## Check dependencies

TRACE 'Checking anyenv is installed.'
if [[ ! -x "${HOME}/.anyenv/bin/anyenv" ]]; then
  FATAL 'Error: rbenv installer requires anyenv.'
  exit 1
fi

# Initialize anyenv.
TRACE 'Initializing anyenv.'
eval "$("${HOME}/.anyenv/bin/anyenv" init - bash)" | TRACE


## Install rbenv

INFO 'Installing rbenv.'
anyenv install -s rbenv | DEBUG

# Initialize rbenv
TRACE 'Initializing rbenv.'
eval "$("${ANYENV_ROOT}/envs/rbenv/bin/rbenv" init - bash)" | TRACE


## Install the stable version of Ruby

ruby_version='2.5.0'

INFO "Installing Ruby v${ruby_version}"
rbenv install -s "$ruby_version" | DEBUG

# Set the installed version of Ruby as default
INFO "Setting Ruby v${ruby_version} as default."
rbenv global "${ruby_version}" | DEBUG


## Install Ruby gems

ruby_gems=(
  bundler
  circle-cli
  fastri
  mdl
  pry
  pry-coolline
  pry-doc
  rake
  rcodetools
  rubocop
  travis
)

INFO 'Installing Ruby gems.'
for gem in "${ruby_gems[@]}"; do
  gem install "$gem" | DEBUG
done

INFO 'Rehashing Ruby gems.'
rbenv rehash | DEBUG
