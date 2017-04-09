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

# ============ Interactive Interfaces ============
zplug 'junegunn/fzf-bin', from:gh-r, use:'*linux*amd64*', as:command, rename-to:fzf
zplug 'junegunn/fzf', as:command, use:'bin/fzf-tmux'
zplug 'junegunn/fzf', use:'shell/key-bindings.zsh'
zplug 'mollifier/anyframe'
zplug 'changyuheng/zsh-interactive-cd'

# ============ Command Assistants ============
zplug 'zdharma/fast-syntax-highlighting', defer:2

# ============ Command Completions ============
zplug 'lib/completion', from:oh-my-zsh
zplug 'zsh-users/zsh-autosuggestions', defer:2
zplug 'zsh-users/zsh-completions'
zplug 'plugins/rust', from:oh-my-zsh
zplug 'plugins/cargo', from:oh-my-zsh
zplug 'plugins/gulp', from:oh-my-zsh
zplug 'plugins/ng', from:oh-my-zsh
zplug 'plugins/react-native', from:oh-my-zsh
zplug 'plugins/gradle', from:oh-my-zsh
zplug 'plugins/spring', from:oh-my-zsh
zplug 'plugins/pip', from:oh-my-zsh
zplug 'plugins/gem', from:oh-my-zsh
zplug 'plugins/rake-fast', from:oh-my-zsh
zplug 'plugins/docker', from:oh-my-zsh
zplug 'plugins/docker-compose', from:oh-my-zsh
zplug 'plugins/kubectl', from:oh-my-zsh
zplug 'plugins/codeclimate', from:oh-my-zsh

# Load zsh configuration
zplug 'plugins/fancy-ctrl-z', from:oh-my-zsh
zplug 'plugins/command-not-found', from:oh-my-zsh

# Use aliases
zplug 'lib/clipboard', from:oh-my-zsh
zplug 'plugins/ubuntu', from:oh-my-zsh, if:'uname -a | grep -q ubuntu'
zplug 'plugins/extract', from:oh-my-zsh
zplug 'plugins/zsh_reload', from:oh-my-zsh

# ============= Version Control ============
zplug 'plugins/gitfast', from:oh-my-zsh
zplug 'plugins/git-flow', from:oh-my-zsh
zplug 'motemen/ghq', dir:"${GOPATH}/src/github.com/motemen/ghq", use:'zsh', hook-build:'go get -u'
zplug 'mollifier/cd-gitroot'
# Use gitignore.io CLI, gitignore
zplug 'plugins/gitignore', from:oh-my-zsh
# Use GitHub Gist CLI, gist
zplug 'b4b4r07/gist', dir:"${GOPATH}/src/github.com/b4b4r07/gist", use:'misc/completion/zsh', hook-build:'go get -u'
# Use a workspace manager, ghq
# Use command to go to root directory of current repo

# ============ Appearance ============
zplug 'bhilburn/powerlevel9k', at:next, as:theme
zplug 'sei40kr/dircolors-solarized', use:'*.ansi-dark.zsh'

# Use alias tips
zplug 'djui/alias-tips'

zplug '~/dev/ws/github.com/sei40kr/zsh-tmux-rename', from:local

zplug load

