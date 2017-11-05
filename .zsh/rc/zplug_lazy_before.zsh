#!/usr/bin/env zsh

# zplug_lazy_before.zsh
# author: Seong Yong-ju ( @sei40kr )

# b4b4r07/emoji-cli {{{
EMOJI_CLI_KEYBIND='^s'

if [[ -n "$TMUX" ]] && [[ "${+commands[fzf-tmux]}" == 1 ]]
then
  EMOJI_CLI_FILTER="fzf-tmux -d ${FZF_TMUX_HEIGHT}"
fi

emoji::cli() {
  unfunction emoji::cli
  . "${ZPLUG_REPOS}/b4b4r07/emoji-cli/emoji-cli.plugin.zsh"

  emoji::cli "$@"
}

zle -N emoji::cli

bindkey "$EMOJI_CLI_KEYBIND" emoji::cli
# }}}

# b4b4r07/enhancd {{{
__enhancd::cd() {
  unalias cd
  unfunction __enhancd::cd
  . "${ZPLUG_REPOS}/b4b4r07/enhancd/init.sh"

  __enhancd::cd "$@"
}

alias cd='__enhancd::cd'
# }}}

# djui/alias-tips {{{
ZSH_PLUGINS_ALIAS_TIPS_TEXT='alias-tips: '
ZSH_PLUGINS_ALIAS_TIPS_FORCE=1

_alias_tips__preexec() {
  unfunction _alias_tips__preexec
  . "${ZPLUG_REPOS}/djui/alias-tips/alias-tips.plugin.zsh"

  _alias_tips__preexec "$@"
}

autoload -Uz add-zsh-hook
add-zsh-hook preexec _alias_tips__preexec
# }}}

# lukechilds/zsh-better-npm-completion {{{
_zbnc_zsh_better_npm_completion() {
  unfunction _zbnc_zsh_better_npm_completion
  . "${ZPLUG_REPOS}/lukechilds/zsh-better-npm-completion/zsh-better-npm-completion.plugin.zsh"

  _zbnc_zsh_better_npm_completion "$@"
}

compdef _zbnc_zsh_better_npm_completion npm
# }}}

# mollifier/anyframe {{{
anyframe-widget-cd-ghq-repository() {
  anyframe-source-ghq-repository | \
      anyframe-selector-auto | \
      anyframe-action-execute builtin cd --
}

zle -N anyframe-widget-cd-ghq-repository
zle -N anyframe-widget-cdr
zle -N anyframe-widget-checkout-git-branch
zle -N anyframe-widget-execute-history
zle -N anyframe-widget-git-add
zle -N anyframe-widget-insert-filename
zle -N anyframe-widget-insert-git-branch
zle -N anyframe-widget-kill
zle -N anyframe-widget-put-history
zle -N anyframe-widget-select-widget
zle -N anyframe-widget-tmux-attach

zstyle ':anyframe:selector:fzf-tmux:' command "fzf-tmux -d ${FZF_TMUX_HEIGHT}"

bindkey '^r' anyframe-widget-execute-history
bindkey '^xb' anyframe-widget-cdr
bindkey '^x^b' anyframe-widget-checkout-git-branch
bindkey '^xr' anyframe-widget-execute-history
bindkey '^x^r' anyframe-widget-execute-history
bindkey '^xi' anyframe-widget-put-history
bindkey '^x^i' anyframe-widget-put-history
bindkey '^xg' anyframe-widget-cd-ghq-repository
bindkey '^x^g' anyframe-widget-cd-ghq-repository
bindkey '^xk' anyframe-widget-kill
bindkey '^x^k' anyframe-widget-kill
bindkey '^xe' anyframe-widget-insert-git-branch
bindkey '^x^e' anyframe-widget-insert-git-branch
# }}}

# mollifier/cd-gitroot {{{
alias U='cd-gitroot'
# }}}

# supercrabtree/k {{{
k() {
  unfunction k
  . "${ZPLUG_REPOS}/supercrabtree/k/k.plugin.zsh"

  k "$@"
}
# }}}

# robbyrussell/oh-my-zsh/plugins/extract {{{
extract() {
  unalias x
  unfunction extract
  . "${ZPLUG_REPOS}/robbyrussell/oh-my-zsh/plugins/extract/extract.plugin.zsh"

  extract "$@"
}

alias x='extract'
# }}}

# robbyrussell/oh-my-zsh/plugins/gradle {{{
_gradle_tasks() {
  unfunction _gradle_tasks _gradlew_tasks
  . "${ZPLUG_REPOS}/robbyrussell/oh-my-zsh/plugins/gradle/gradle.plugin.zsh"

  _gradle_tasks "$@"
}

_gradlew_tasks() {
  unfunction _gradle_tasks _gradlew_tasks
  . "${ZPLUG_REPOS}/robbyrussell/oh-my-zsh/plugins/gradle/gradle.plugin.zsh"

  _gradlew_tasks "$@"
}

compdef _gradle_tasks gradle
compdef _gradlew_tasks gradlew
# }}}

# robbyrussell/oh-my-zsh/plugins/gulp {{{
$$gulp_completion() {
  unfunction $$gulp_completion
  . "${ZPLUG_REPOS}/robbyrussell/oh-my-zsh/plugins/gulp/gulp.plugin.zsh"

  $$gulp_completion "$@"
}

compdef $$gulp_completion gulp
# }}}

# robbyrussell/oh-my-zsh/plugins/ng {{{
_ng_completion() {
  unfunction _ng_completion
  . "${ZPLUG_REPOS}/robbyrussell/oh-my-zsh/plugins/ng/ng.plugin.zsh"

  _ng_completion "$@"
}

compdef _ng_completion ng
# }}}

# robbyrussell/oh-my-zsh/plugins/rake-fast {{{
_rake() {
  unfunction _rake
  . "${ZPLUG_REPOS}/robbyrussell/oh-my-zsh/plugins/rake-fast/rake-fast.plugin.zsh"

  _rake "$@"
}

compdef _rake rake
# }}}
