#!/usr/bin/env zsh

# anyenv_setup.zsh
# author: Seong Yong-ju ( @sei40kr )

NODE_INSTALL_VERSION='v8.9.4'
PYTHON2_INSTALL_VERSION='2.7.14'
PYTHON3_INSTALL_VERSION='3.6.4'
RUBY_INSTALL_VERSION='2.4.3'

typeset -a envs
envs=( crenv goenv ndenv phpenv plenv pyenv rbenv )

for env in envs
do
  anyenv install -s "$env"
done
