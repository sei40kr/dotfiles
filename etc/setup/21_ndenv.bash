#!/usr/bin/env bash

# 21_ndenv.bash - ndenv installer
# author: Seong Yong-ju <sei40kr@gmail.com>

set -euo pipefail
IFS=$'\n\t'


## Check dependencies

if [[ ! -x "${HOME}/.anyenv/bin/anyenv" ]]; then
  echo 'Error: ndenv installer requires anyenv.' 1>&2
  exit 1
fi

# Initialize anyenv.
eval "$("${HOME}/.anyenv/bin/anyenv" init - bash)"


## Install ndenv

echo 'Info: Installing ndenv.'
anyenv install -s ndenv

# Initialize ndenv
eval "$("${ANYENV_ROOT}/envs/ndenv/bin/ndenv" init - bash)"


## Install the stable version of Node

node_version='v8.10.0'

echo "Info: Installing Node ${node_version}"
ndenv install -s "$node_version"

# Set the installed version of Node as default
ndenv global "${node_version}"


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
  yaml-language-server
)

echo 'Info: Installing NPM packages'

# Use yarn command if available. Add --global-folder option to tell yarn
# destination directory.
# cf https://github.com/yarnpkg/yarn/issues/1027
if command -v yarn 2>/dev/null; then
  yarn global add "${npm_packages[@]}" --global-folder="$(yarn global bin)"
else
  npm install -g "${npm_packages[@]}"
fi

ndenv rehash
