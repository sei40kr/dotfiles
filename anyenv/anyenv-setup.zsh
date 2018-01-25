#!/usr/bin/env zsh

# anyenv_setup.zsh
# author: Seong Yong-ju ( @sei40kr )

CRYSTAL_INSTALL_VERSION='0.24.1'
GO_INSTALL_VERSION='8.9.4'
NODE_INSTALL_VERSION='v8.9.4'
PYTHON2_INSTALL_VERSION='2.7.14'
PYTHON3_INSTALL_VERSION='3.6.4'
RUBY_INSTALL_VERSION='2.4.3'

[[ "${+commands[anyenv]}" == 1 ]] || {
  -log-wait 'Installing anyenv ...'

  [[ -d "${HOME}/.anyenv" ]] || git clone https://github.com/riywo/anyenv \
      "${HOME}/.anyenv"
  typeset -u path
  path=( "${HOME}/.anyenv/bin" "${path[@]}" )
  rehash && eval "$(anyenv init -)"

  -log-done 'anyenv was successfully installed.'
}

# Install version managers
typeset -a envs
envs=( crenv goenv ndenv phpenv plenv pyenv rbenv )

for env in "${envs[@]}"
do
  -log-wait "Installing ${env} ..."

  anyenv install -s "$env"

  -log-done "${env} was successfully installed."
done

# Install pyenv-virtualenv
[ -d "${ANYENV_ROOT}/envs/pyenv/plugins/pyenv-virtualenv" ] || {
  -log-wait "Installing pyenv-virtualenv ..."

  git clone https://github.com/pyenv/pyenv-virtualenv.git \
      "${ANYENV_ROOT}/envs/pyenv/plugins/pyenv-virtualenv"

  -log-done "pyenv-virtualenv was successfully installed."
}

rehash && eval "$(anyenv init -)"

# Install and setup Node
-log-wait "Installing Node ${NODE_INSTALL_VERSION} ..."

ndenv install -s "$NODE_INSTALL_VERSION"
ndenv global "$NODE_INSTALL_VERSION"

-log-done "Node ${NODE_INSTALL_VERSION} were successfully installed."

# Install and setup Python 2,3
-log-wait \
    "Installing Python v${PYTHON2_INSTALL_VERSION} and v${PYTHON_INSTALL_VERSION3} ..."

pyenv install -s "$PYTHON2_INSTALL_VERSION"
pyenv install -s "$PYTHON3_INSTALL_VERSION"
pyenv global "$PYTHON3_INSTALL_VERSION" "$PYTHON2_INSTALL_VERSION"

-log-done \
    "Python v${PYTHON2_INSTALL_VERSION} and v${PYTHON_INSTALL_VERSION3} were successfully installed."

# Install and setup Ruby
-log-wait "Installing Ruby v${RUBY_INSTALL_VERSION} ..."

rbenv install -s "$RUBY_INSTALL_VERSION"
rbenv global "$RUBY_INSTALL_VERSION"

-log-done "Ruby v${RUBY_INSTALL_VERSION} were successfully installed."
