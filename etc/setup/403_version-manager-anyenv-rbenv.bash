#!/usr/bin/env bash

# 403_version-manager-anyenv-rbenv.bash - rbenv installer
# author: Seong Yong-ju <sei40kr@gmail.com>

set -euo pipefail
IFS=$'\n\t'


## Check dependencies

if [[ ! -x "${HOME}/.anyenv/bin/anyenv" ]]; then
  echo 'Error: rbenv installer requires anyenv.' 1>&2
  exit 1
fi

# Initialize anyenv.
eval "$("${HOME}/.anyenv/bin/anyenv" init - bash)"


## Install rbenv

echo 'Info: Installing rbenv.'
anyenv install -s rbenv

# Initialize rbenv
eval "$("${ANYENV_ROOT}/envs/rbenv/bin/rbenv" init - bash)"


## Install the stable version of Ruby

ruby_version='2.5.0'

echo "Info: Installing Ruby v${ruby_version}"
rbenv install -s "$ruby_version"

# Set the installed version of Ruby as default
rbenv global "${ruby_version}"


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

echo "Info: Installing Ruby gems."
for gem in $ruby_gems; do
  gem install "$gem"
done

rbenv rehash
