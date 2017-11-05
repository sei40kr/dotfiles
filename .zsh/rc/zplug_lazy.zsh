#!/usr/bin/env zsh

# zplug_lazy.zsh
# author: Seong Yong-ju ( @sei40kr )

zplug 'mollifier/anyframe',         use:'anyframe-functions/*/*',  lazy:true
zplug 'mollifier/cd-gitroot',       use:'cd-gitroot,_cd-gitroot',  lazy:true
zplug 'zsh-users/zsh-completions',  use:'src/_*',                  lazy:true

zplug 'plugins/autopep8',      from:oh-my-zsh,  use:'_autopep8',      lazy:true
zplug 'plugins/cargo',         from:oh-my-zsh,  use:'_cargo',         lazy:true
zplug 'plugins/codeclimate',   from:oh-my-zsh,  use:'_codeclimate',   lazy:true
zplug 'plugins/gem',           from:oh-my-zsh,  use:'_gem',           lazy:true
zplug 'plugins/gitfast',       from:oh-my-zsh,  use:'_git',           lazy:true
zplug 'plugins/pip',           from:oh-my-zsh,  use:'_pip',           lazy:true
zplug 'plugins/rust',          from:oh-my-zsh,  use:'_rust',          lazy:true
zplug 'plugins/react-native',  from:oh-my-zsh,  use:'_react-native',  lazy:true
zplug 'plugins/spring',        from:oh-my-zsh,  use:'_spring',        lazy:true

zplug 'b4b4r07/gotcha', from:gh-r, as:command

zplug 'b4b4r07/enhancd',                       use:'!*'
zplug 'b4b4r07/emoji-cli',                     use:'!*'
zplug 'lukechilds/zsh-better-npm-completion',  use:'!*'
zplug 'djui/alias-tips',                       use:'!*'
zplug 'supercrabtree/k',                       use:'!*'
