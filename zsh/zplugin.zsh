zplugin ice svn; zplugin snippet PZT::modules/helper


## Commands

zplugin ice as'program' pick'copy' mv'copy.sh -> copy'
zplugin light b4b4r07/copy

zplugin ice from'gh-r' as'program' mv'gotcha_* -> gotcha'
zplugin light b4b4r07/gotcha

zplugin ice as'program' pick'httpstat' mv'httpstat.sh -> httpstat'
zplugin light b4b4r07/httpstat

zplugin ice as'program' pick'bin/ssh-keyreg'; zplugin light b4b4r07/ssh-keyreg
zplugin ice as'program' pick'bin/gomi'; zplugin light b4b4r07/zsh-gomi
zplugin ice as'program' pick'*shrc'; zplugin light Russell91/sshrc


## ZSH functions

zplugin ice atload'alias U="cd-gitroot"'
zplugin light mollifier/cd-gitroot

zplugin ice pick'k.sh'; zplugin light supercrabtree/k
alias k="k -Ah --no-vcs"


## ZSH environments

# Prezto
zplugin snippet PZT::modules/environment/init.zsh

zstyle ':prezto:module:completion:*:hosts' etc-host-ignores \
       '0.0.0.0' '127.0.0.1'
zplugin ice blockf; zplugin snippet PZT::modules/completion/init.zsh

zstyle ':prezto:module:editor' key-bindings emacs
zstyle ':prezto:module:editor' dot-expansion yes
zplugin snippet PZT::modules/editor/init.zsh

zplugin snippet PZT::modules/directory/init.zsh
zplugin snippet PZT::modules/history/init.zsh
zplugin snippet PZT::modules/gnu-utility/init.zsh

# Oh My Zsh
zplugin snippet OMZ::plugins/fancy-ctrl-z/fancy-ctrl-z.plugin.zsh

## Completions and aliases

zplugin ice blockf pick''; zplugin light zsh-users/zsh-completions

# Prezto
zplugin ice svn pick'init.zsh'; zplugin snippet PZT::modules/docker
zplugin snippet PZT::modules/homebrew/init.zsh
zplugin snippet PZT::modules/rsync/init.zsh
zplugin ice svn pick'init.zsh'; zplugin snippet PZT::modules/ssh

# Oh My Zsh
zplugin ice wait''; zplugin snippet OMZ::plugins/ant/ant.plugin.zsh
zplugin ice svn; zplugin snippet OMZ::plugins/autopep8
zplugin ice svn; zplugin snippet OMZ::plugins/bundler
zplugin ice svn; zplugin snippet OMZ::plugins/capistrano
zplugin snippet OMZ::plugins/docker-compose/docker-compose.plugin.zsh
zplugin ice svn; zplugin snippet OMZ::plugins/extract
zplugin ice svn; zplugin snippet OMZ::plugins/gem
zplugin snippet OMZ::plugins/git/git.plugin.zsh
zplugin ice svn; zplugin snippet OMZ::plugins/golang
zplugin ice svn wait''; zplugin snippet OMZ::plugins/gradle
zplugin ice wait''; zplugin snippet OMZ::plugins/mosh/mosh.plugin.zsh
zplugin snippet OMZ::plugins/mvn/mvn.plugin.zsh
zplugin snippet OMZ::plugins/npm/npm.plugin.zsh
zplugin ice svn; zplugin snippet OMZ::plugins/pip
zplugin snippet OMZ::plugins/postgres/postgres.plugin.zsh
zplugin ice svn atload 'unalias rg'; zplugin snippet OMZ::plugins/rails
zplugin snippet OMZ::plugins/rake/rake.plugin.zsh
zplugin ice wait''; zplugin snippet OMZ::plugins/rake-fast/rake-fast.plugin.zsh
zplugin snippet OMZ::plugins/ruby/ruby.plugin.zsh
zplugin ice wait''; zplugin snippet OMZ::plugins/stack/stack.plugin.zsh
zplugin snippet OMZ::plugins/yarn/yarn.plugin.zsh
zplugin ice svn; zplugin snippet OMZ::plugins/react-native

# docker
zplugin ice wait''
zplugin snippet https://raw.githubusercontent.com/docker/docker-ce/master/components/cli/contrib/completion/zsh/_docker
# docker-compose
zplugin ice wait''
zplugin snippet https://raw.githubusercontent.com/docker/compose/master/contrib/completion/zsh/_docker-compose

# jsforce
zplugin ice pick'' wait''; zplugin light jsforce/jsforce-zsh-completions

# npm
zplugin ice wait''; zplugin light lukechilds/zsh-better-npm-completion


## ZSH theme

zplugin ice pick'async.zsh' lucid src'pure.zsh' wait'!0'
zplugin light sindresorhus/pure


## ZSH enhancements

zplugin snippet OMZ::plugins/colored-man-pages/colored-man-pages.plugin.zsh

export ENHANCD_FILTER="fzf --height 50% --reverse"
export ENHANCD_DOT_SHOW_FULLPATH=1
zplugin ice pick'init.sh'; zplugin light b4b4r07/enhancd

zplugin ice lucid atinit'
export ZSH_PLUGINS_ALIAS_TIPS_TEXT="alias-tips: "' wait'1'
zplugin light djui/alias-tips

zplugin ice lucid wait'1' load'[[ -n "$TMUX" ]]' unload'[[ -z "$TMUX" ]]'
zplugin load sei40kr/zsh-tmux-rename

zplugin ice lucid atload'_zsh_autosuggest_start' wait'0'
zplugin light zsh-users/zsh-autosuggestions

zplugin ice lucid atload'zpcompinit' wait'0'
zplugin light zdharma/fast-syntax-highlighting
