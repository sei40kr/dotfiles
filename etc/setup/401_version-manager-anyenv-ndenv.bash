#!/usr/bin/env bash

# 401_version-manager-anyenv-ndenv.bash - ndenv installer
# author: Seong Yong-ju <sei40kr@gmail.com>

set -euo pipefail
IFS=$'\n\t'

# shellcheck source=helpers/init-helpers.bash
. "$(dirname "${BASH_SOURCE[0]}")/helpers/init-helpers.bash"


## Check dependencies

TRACE 'Checking anyenv is installed.'
if [[ ! -x "${HOME}/.anyenv/bin/anyenv" ]]; then
  FATAL 'ndenv installer requires anyenv. Install anyenv first.'
  exit 1
fi

# Initialize anyenv.
TRACE 'Initializing anyenv.'
eval "$("${HOME}/.anyenv/bin/anyenv" init - bash)" | TRACE


## Install ndenv

INFO 'Installing ndenv.'
anyenv install -s ndenv | DEBUG

# Initialize ndenv
TRACE 'Initializing ndenv.'
eval "$("${ANYENV_ROOT}/envs/ndenv/bin/ndenv" init - bash)" | TRACE


## Install the stable version of Node

node_version='v8.10.0'

INFO "Installing Node ${node_version}"
ndenv install -s "$node_version" | DEBUG

# Set the installed version of Node as default
INFO "Setting Node ${node_version} as default."
ndenv global "${node_version}" | DEBUG


## Install NPM packages

npm_packages=(
  create-react-app
  create-react-native-app
  eslint-cli
  eslint_d
  flow-bin
  flow-language-server
  gatsby-cli
  generate
  generate-gitignore
  generate-license
  generate-project
  gulp
  import-js
  jsforce
  markdownlint-cli
  prettier
  prettier-eslint-cli
  sfdx-cli
  stylelint-cli
  typescript
  typescript-language-server
  vmd
  webpack
)

INFO 'Installing NPM packages'

# Run yarn command if its available.
# Add --global-folder option to tell yarn the binary directory.
# cf https://github.com/yarnpkg/yarn/issues/1027
if command -v yarn 2>/dev/null; then
  yarn global add "${npm_packages[@]}" --global-folder="$(yarn global bin)" \
    | DEBUG
else
  npm install -g "${npm_packages[@]}" | DEBUG
fi

INFO 'Rehashing NPM packages.'
ndenv rehash | DEBUG
