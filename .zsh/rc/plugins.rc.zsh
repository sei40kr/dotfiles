#!/usr/bin/env zsh

# plugins.rc.zsh
# author: Seong Yong-ju ( @sei40kr )

export ZPLUG_HOME="${HOME}/.zplug"
# Install zplug if it's not installed
[ ! -d "$ZPLUG_HOME" ] && {
  echo 'Installing zplug ...';
  git clone --depth 1 https://github.com/zplug/zplug.git "$ZPLUG_HOME";
}

# Load zplug
source "${ZPLUG_HOME}/init.zsh"

zplug 'denysdovhan/spaceship-zsh-theme', use:spaceship.zsh, as:theme
zplug 'djui/alias-tips'
zplug 'junegunn/fzf', hook-build:'./install --key-bindings --completion --no-update-rc'
zplug 'lib/clipboard', from:oh-my-zsh
zplug 'lib/completion', from:oh-my-zsh
zplug 'lukechilds/zsh-nvm'
zplug 'mollifier/anyframe'
zplug 'mollifier/cd-gitroot'
zplug 'motemen/ghq', dir:"${GOPATH}/src/github.com/motemen/ghq", use:'zsh', hook-build:'go get -u'
zplug 'plugins/cargo', from:oh-my-zsh
zplug 'plugins/codeclimate', from:oh-my-zsh
zplug 'plugins/command-not-found', from:oh-my-zsh
zplug 'plugins/docker', from:oh-my-zsh
zplug 'plugins/docker-compose', from:oh-my-zsh
zplug 'plugins/extract', from:oh-my-zsh
zplug 'plugins/fancy-ctrl-z', from:oh-my-zsh
zplug 'plugins/gem', from:oh-my-zsh
zplug 'plugins/git-flow', from:oh-my-zsh
zplug 'plugins/gitfast', from:oh-my-zsh
zplug 'plugins/gitignore', from:oh-my-zsh
zplug 'plugins/gradle', from:oh-my-zsh
zplug 'plugins/gulp', from:oh-my-zsh
zplug 'plugins/kubectl', from:oh-my-zsh
zplug 'plugins/ng', from:oh-my-zsh
zplug 'plugins/pip', from:oh-my-zsh
zplug 'plugins/pyenv', from:oh-my-zsh
zplug 'plugins/rake-fast', from:oh-my-zsh
zplug 'plugins/rbenv', from:oh-my-zsh
zplug 'plugins/react-native', from:oh-my-zsh
zplug 'plugins/rust', from:oh-my-zsh
zplug 'plugins/spring', from:oh-my-zsh
zplug 'plugins/ubuntu', from:oh-my-zsh, if:'uname -a | grep -q ubuntu'
zplug 'plugins/zsh_reload', from:oh-my-zsh
zplug 'sei40kr/dircolors-solarized', use:'*.ansi-dark.zsh'
zplug 'supercrabtree/k'
zplug 'zdharma/fast-syntax-highlighting', defer:2
zplug 'zsh-users/zsh-autosuggestions', defer:2
zplug 'zsh-users/zsh-completions'

zplug '~/dev/ws/github.com/sei40kr/zsh-tmux-rename', from:local

zplug load

