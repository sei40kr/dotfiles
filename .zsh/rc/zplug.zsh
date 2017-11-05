#!/usr/bin/env zsh

# zplug.zsh
# author: Seong Yong-ju ( @sei40kr )

zplug 'lib/completion',             from:oh-my-zsh
zplug 'lib/compfix',                from:oh-my-zsh
zplug 'lib/clipboard',              from:oh-my-zsh
zplug 'lib/key-bindings',           from:oh-my-zsh
zplug 'plugins/command-not-found',  from:oh-my-zsh
zplug 'plugins/gnu-utils',          from:oh-my-zsh

zplug 'lukechilds/zsh-nvm'

zplug 'plugins/brew',               from:oh-my-zsh
zplug 'plugins/colored-man-pages',  from:oh-my-zsh
zplug 'plugins/common-aliases',     from:oh-my-zsh
zplug 'plugins/dotenv',             from:oh-my-zsh
zplug 'plugins/fancy-ctrl-z',       from:oh-my-zsh
zplug 'plugins/gem',                from:oh-my-zsh
zplug 'plugins/git',                from:oh-my-zsh
zplug 'plugins/gitignore',          from:oh-my-zsh
zplug 'plugins/jsontools',          from:oh-my-zsh
zplug 'plugins/pip',                from:oh-my-zsh
zplug 'plugins/rake',               from:oh-my-zsh
zplug 'plugins/react-native',       from:oh-my-zsh
zplug 'plugins/zsh_reload',         from:oh-my-zsh
zplug 'plugins/archlinux',          from:oh-my-zsh,  if:'[[ "${+commands[pacman]}" == 1 ]]'
zplug 'plugins/ubuntu',             from:oh-my-zsh,  if:'[[ "${+commands[apt-get]}" == 1 ]]'

zplug "${HOME}/.linuxbrew",  from:local,  use:'opt/fzf/shell/*.zsh'
zplug "${HOME}/.sdkman",     from:local,  use:'bin/sdkman-init.sh',  if:'[[ -d "${HOME}/.sdkman" ]]'

zplug 'zdharma/fast-syntax-highlighting',  defer:2
zplug 'zsh-users/zsh-autosuggestions',     defer:2,  use:'zsh-autosuggestions.zsh'

zplug 'denysdovhan/spaceship-zsh-theme', as:theme, defer:3, use:'spaceship.zsh'
