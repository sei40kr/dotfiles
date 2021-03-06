# author: Seong Yong-ju <sei40kr@gmail.com>

autoload -Uz \
          run-help-git \
          pyclean

if [[ "$INSIDE_EMACS" == vterm ]]; then
    alias clear='vterm_printf "51; Evterm-clear-scrollback"; tput clear'
fi

HYPHEN_INSENSITIVE=true
zinit snippet OMZL::clipboard.zsh
zinit snippet OMZL::completion.zsh

zinit snippet OMZP::zsh_reload/zsh_reload.plugin.zsh

zinit ice pick'' blockf wait''
zinit light zsh-users/zsh-completions

zinit ice trigger-load'!cd-gitroot'
zinit light mollifier/cd-gitroot
zinit ice trigger-load'!extract;!x'
zinit snippet OMZP::extract/extract.plugin.zsh
zinit snippet OMZP::rsync/rsync.plugin.zsh
zinit snippet OMZP::nmap/nmap.plugin.zsh


## Perl

zinit snippet OMZP::perl/perl.plugin.zsh
zinit ice as'completion' wait''
zinit snippet OMZP::cpanm/_cpanm


## PHP

zinit snippet OMZP::composer/composer.plugin.zsh
zinit snippet OMZP::laravel/laravel.plugin.zsh
zinit snippet OMZP::laravel5/laravel5.plugin.zsh


## Web Frontend

zinit ice wait''
zinit snippet OMZP::gulp/gulp.plugin.zsh

# Gatsby
zinit ice as'completion' wait''
zinit snippet OMZP::gatsby/_gatsby

# React Native
zinit ice svn
zinit snippet OMZP::react-native

# Flutter
zinit ice svn
zinit snippet OMZP::flutter


## Database

# Redis
zinit ice as'completion' wait''
zinit snippet OMZP::redis-cli/_redis-cli


## Continuous Integration

# Code Climate
zinit ice as'completion' wait''
zinit snippet OMZP::codeclimate/_codeclimate


## Infrastructure

zinit ice as'completion' wait''
zinit snippet OMZP::vagrant/_vagrant
zinit ice as'completion' wait''
zinit snippet OMZP::terraform/_terraform
zinit snippet OMZP::kubectl/kubectl.plugin.zsh


#
# Completions

zinit wait'' lucid atpull'%atclone' as'completion' for \
    has'karma'   atclone'karma completion >_karma'             id-as'karma_completion'   zdharma/null \
    has'kubectl' atclone'kubectl completion zsh >_kubectl'     id-as'kubectl_completion' zdharma/null


## Others

zinit ice wait'0' atinit'zpcompinit; zpcdreplay' lucid
zinit light zdharma/fast-syntax-highlighting

zinit ice wait'0' lucid
zinit light -b hlissner/zsh-autopair

zinit ice wait'0' lucid
zinit light -b zsh-users/zsh-autosuggestions

zinit ice wait'0' lucid
zinit light -b zdharma/history-search-multi-word

if [[ "${+aliases[run-help]}" == 1 ]]; then
    unalias run-help
fi

zinit snippet OMZP::fancy-ctrl-z/fancy-ctrl-z.plugin.zsh

# FZF
FZF_DEFAULT_OPTS='--height=15 --reverse --inline-info --color=dark --color=fg:-1,bg:-1,hl:#c678dd,fg+:#ffffff,bg+:#4b5263,hl+:#d858fe --color=info:#98c379,prompt:#61afef,pointer:#be5046,marker:#e5c07b,spinner:#61afef,header:#61afef'
zinit ice has'fzf' bindmap'^R ->;\ec ->' multisrc'shell/{completion,key-bindings}.zsh'
zinit light -b junegunn/fzf

# Notification
zinit snippet OMZP::bgnotify/bgnotify.plugin.zsh

## Theme & Appearance

PURE_PROMPT_SYMBOL='λ'
PURE_GIT_DOWN_ARROW='▾'
PURE_GIT_UP_ARROW='▴'
zstyle :prompt:pure:prompt:success color yellow
zinit ice compile'(pure|async).zsh' pick'async.zsh' src'pure.zsh'
zinit light sindresorhus/pure

export YSU_MESSAGE_FORMAT="💡 You should use: $(tput bold)%alias$(tput sgr0)"
export YSU_HARDCORE=1
zinit light MichaelAquilina/zsh-you-should-use
