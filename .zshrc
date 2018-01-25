#!/usr/bin/env zsh

# .zshrc
# author: Seong Yong-ju ( @sei40kr )

. "${HOME}/.zsh/rc/exports.rc.zsh"

zmodload zsh/zpty

if [[ "${+commands[anyenv]}" == 1 ]]
then
  eval "$(anyenv init - zsh)"
fi

if [[ -s "${HOME}/.sdkman/bin/sdkman-init.sh" ]]
then
  . "${HOME}/.sdkman/bin/sdkman-init.sh"
fi

autoload -Uz compinit

. "${HOME}/.zplugin/bin/zplugin.zsh"

autoload -Uz add-zsh-hook cdr chpwd_recent_dirs _zplugin

if [[ "${+_comps}" == 1 ]]
then
  _comps[zplugin]=_zplugin
fi

zplugin snippet "${HOME}/.zsh/rc/10_utilities.zsh"
zplugin snippet "${HOME}/.zsh/rc/20_key-bindings.zsh"
zplugin snippet "${HOME}/.zsh/rc/30_aliases.zsh"
zplugin snippet "${HOME}/.zsh/rc/50_options.zsh"
zplugin snippet "${HOME}/.zsh/rc/70_misc.zsh"
zplugin snippet "${HOME}/.zsh/rc/80_custom.zsh"

## Environments
# Launch tmux if not running
zstyle ':prezto:module:tmux:auto-start' local 'yes'
zstyle ':prezto:module:tmux:session' name 'default'
zplugin ice svn; zplugin snippet PZT::modules/tmux

# Setup Oh My Zsh
ZSH="${HOME}/.zsh"
ZSH_CUSTOM="${ZSH}/custom"
ZSH_CACHE_DIR="${HOME}/.cache/zsh"
typeset -U fpath
fpath=( "${ZSH}/completions" "${ZSH}/functions" "${fpath[@]}" )
[[ -d "$ZSH_CACHE_DIR" ]] || mkdir -p "$ZSH_CACHE_DIR"

# Setup Prezto
zplugin ice svn; zplugin snippet PZT::modules/helper

zplugin snippet PZT::modules/environment/init.zsh

zplugin snippet PZT::modules/directory/init.zsh
zplugin snippet PZT::modules/history/init.zsh

## Completions and aliases
zplugin snippet OMZ::plugins/common-aliases/common-aliases.plugin.zsh
# Remove unwanted aliases
unalias rm cp mv

zstyle ':prezto:module:completion:*:hosts' etc-host-ignores \
    '0.0.0.0' '127.0.0.1'
zplugin ice blockf; zplugin snippet PZT::modules/completion/init.zsh

zplugin ice svn pick'init.zsh'; zplugin snippet PZT::modules/docker
zplugin ice svn pick'init.zsh'; zplugin snippet PZT::modules/ssh
zplugin snippet PZT::modules/rsync/init.zsh
zplugin snippet PZT::modules/homebrew/init.zsh

zplugin ice svn; zplugin snippet OMZ::plugins/rails
# Remove unwanted alias
unalias rg

zplugin ice svn; zplugin snippet OMZ::plugins/autopep8
zplugin ice svn; zplugin snippet OMZ::plugins/bundler
zplugin ice svn; zplugin snippet OMZ::plugins/capistrano
zplugin ice svn; zplugin snippet OMZ::plugins/docker-compose
zplugin ice svn; zplugin snippet OMZ::plugins/extract
zplugin ice svn; zplugin snippet OMZ::plugins/gem
zplugin ice svn; zplugin snippet OMZ::plugins/golang
zplugin ice svn; zplugin snippet OMZ::plugins/python
zplugin ice svn; zplugin snippet OMZ::plugins/react-native
zplugin ice svn; zplugin snippet OMZ::plugins/sbt
zplugin snippet OMZ::plugins/ant/ant.plugin.zsh
zplugin snippet OMZ::plugins/composer/composer.plugin.zsh
zplugin snippet OMZ::plugins/dircycle/dircycle.plugin.zsh
zplugin snippet OMZ::plugins/dotenv/dotenv.plugin.zsh
zplugin snippet OMZ::plugins/fancy-ctrl-z/fancy-ctrl-z.plugin.zsh
zplugin snippet OMZ::plugins/git/git.plugin.zsh
zplugin snippet OMZ::plugins/gradle/gradle.plugin.zsh
zplugin snippet OMZ::plugins/kubectl/kubectl.plugin.zsh
zplugin snippet OMZ::plugins/mosh/mosh.plugin.zsh
zplugin snippet OMZ::plugins/mvn/mvn.plugin.zsh
zplugin snippet OMZ::plugins/npm/npm.plugin.zsh
zplugin snippet OMZ::plugins/postgres/postgres.plugin.zsh
zplugin snippet OMZ::plugins/rake/rake.plugin.zsh
zplugin snippet OMZ::plugins/ruby/ruby.plugin.zsh
zplugin snippet OMZ::plugins/stack/stack.plugin.zsh
zplugin snippet OMZ::plugins/tig/tig.plugin.zsh
zplugin snippet OMZ::plugins/yarn/yarn.plugin.zsh
zplugin snippet OMZ::plugins/zsh_reload/zsh_reload.plugin.zsh

zplugin ice blockf; zplugin light zsh-users/zsh-completions

if [[ "$OSTYPE" == darwin* ]]
then
  # macOS
  zplugin ice svn; zplugin snippet PZT::modules/osx
elif [[ "${+commands[apt]}" == 1 ]] && [[ "${+commands[lsb_release]}" == 1 ]]
then
  LSB_RELEASE="$(lsb_release -is)"

  # Debian
  zplugin ice if'[[ "$LSB_RELEASE" == "Debian" ]]'
  zplugin snippet OMZ::plugins/debian/debian.plugin.zsh

  # Ubuntu
  zplugin ice if'[[ "$LSB_RELEASE" =~ ^(Ubuntu|elementary)$ ]]'
  zplugin snippet OMZ::plugins/ubuntu/ubuntu.plugin.zsh
fi

## Commands and UI widgets
ENHANCD_FILTER=fzf
alias u='command cd ..'
zplugin ice pick'init.sh'; zplugin light b4b4r07/enhancd

export TMUXIFIER_LAYOUT_PATH="${HOME}/.tmux-layouts"
zplugin ice pick'init.sh' atinit'ln -sf "$PWD" "${HOME}/.tmuxifier"'
zplugin light jimeh/tmuxifier

zplugin ice pick'k.sh'; zplugin light supercrabtree/k
zplugin light mollifier/anyframe

zplugin snippet \
    'https://raw.githubusercontent.com/junegunn/fzf/master/shell/key-bindings.zsh'

## Executables
zplugin ice from'gh-r' as'command' mv'gotcha_* -> gotcha'
zplugin light b4b4r07/gotcha

zplugin ice cp'httpstat.sh -> httpstat'
zplugin snippet --command \
    'https://raw.githubusercontent.com/b4b4r07/httpstat/master/httpstat.sh'

zplugin ice svn make'diff-highlight' pick'diff-highlight'
zplugin snippet --command \
    'https://github.com/git/git/trunk/contrib/diff-highlight'

zplugin snippet --command \
    'https://raw.githubusercontent.com/jonas/tig/master/contrib/tig-pick'

zplugin snippet --command \
    'https://raw.githubusercontent.com/Russell91/sshrc/master/moshrc'
zplugin snippet --command \
    'https://raw.githubusercontent.com/Russell91/sshrc/master/sshrc'

## Lazy plugins
export ZSH_PLUGINS_ALIAS_TIPS_TEXT='alias-tips: '
zplugin ice wait'1'; zplugin light djui/alias-tips

zplugin ice wait'1'
zplugin light zsh-users/zsh-autosuggestions
zplugin ice wait'1'
zplugin light zdharma/fast-syntax-highlighting

## Theme
zplugin light mafredri/zsh-async
zplugin ice pick'pure.zsh' wait'!0'; zplugin light sindresorhus/pure

compinit
zplugin cdreplay -q
