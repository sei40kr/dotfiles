#!/usr/bin/env zsh

# .zshrc
# author: Seong Yong-ju ( @sei40kr )

if [[ -z "$TMUX" ]] && [[ "${+commands[tmux]}" == 1 ]]
then
  export GOPATH="${HOME}/.go"
  export ANYENV_ROOT="${HOME}/.anyenv"

  export HOMEBREW_PREFIX="$(brew --prefix)"

  typeset -U path PATH

  export path=(
    "${HOMEBREW_PREFIX}/bin"
    "${HOME}/.cabal/bin"
    "${HOME}/.cargo/bin"
    "${GOPATH}/bin"
    "${ANYENV_ROOT}/bin"

    "${path[@]}"
  )

  export XDG_CONFIG_HOME="${HOME}/.config"
  export XDG_DATA_DIRS="${HOMEBREW_PREFIX}/share:${XDG_DATA_DIRS}"

  eval "$(anyenv init - zsh)"

  tmux new-session
  exit
fi

zmodload zsh/zpty

autoload -Uz compinit

. "${HOME}/.zplugin/bin/zplugin.zsh"

autoload -Uz _zplugin

autoload -Uz add-zsh-hook
autoload -Uz cdr
autoload -Uz chpwd_recent_dirs

if [[ "${+_comps}" == true ]]
then
  _comps[zplugin]=_zplugin
fi

# zplugin: Utilities {{{
zplugin snippet 'OMZ::lib/git.zsh'
zplugin snippet 'OMZ::lib/clipboard.zsh'
zplugin snippet "${HOME}/.zsh/rc/10_utilities.zsh"

zplugin snippet 'OMZ::lib/key-bindings.zsh'
zplugin snippet "${HOMEBREW_PREFIX}/opt/fzf/shell/key-bindings.zsh"
zplugin snippet "${HOME}/.zsh/rc/20_key-bindings.zsh"

zplugin snippet "${HOME}/.zsh/rc/30_aliases.zsh"

zplugin snippet 'OMZ::lib/completion.zsh'
zplugin snippet 'OMZ::lib/compfix.zsh'
zplugin snippet "${HOMEBREW_PREFIX}/opt/fzf/shell/completion.zsh"
zplugin snippet "${HOME}/.zsh/rc/50_options.zsh"

zplugin ice pick'async.zsh'; zplugin light 'mafredri/zsh-async'
zplugin snippet "${HOME}/.zsh/rc/70_misc.zsh"

zplugin snippet "${HOME}/.zsh/rc/80_custom.zsh"
# }}}

# zplugin: Plugins {{{

# djui/alias-tips {{{
export ZSH_PLUGINS_ALIAS_TIPS_TEXT='alias-tips: '
# }}}

# lukechilds/zsh-nvm {{{
NVM_SYMLINK_CURRENT=true
NVM_LAZY_LOAD=true
NVM_AUTO_USE=true
# }}}

# sindresorhus/pure {{{
PURE_PROMPT_SYMBOL='➔'
# }}}

zplugin light 'b4b4r07/emoji-cli'
zplugin light 'b4b4r07/enhancd'
zplugin light 'djui/alias-tips'
zplugin light 'lukechilds/zsh-nvm'
zplugin light 'mollifier/anyframe'
zplugin light 'mollifier/cd-gitroot'
zplugin light 'sei40kr/zsh-tmux-rename'
zplugin ice pick'k.sh'; zplugin light 'supercrabtree/k'

zplugin snippet 'OMZ::plugins/dotenv/dotenv.plugin.zsh'
zplugin snippet 'OMZ::plugins/fancy-ctrl-z/fancy-ctrl-z.plugin.zsh'
zplugin snippet 'OMZ::plugins/git/git.plugin.zsh'
zplugin snippet 'OMZ::plugins/gnu-utils/gnu-utils.plugin.zsh'
zplugin snippet 'OMZ::plugins/jsontools/jsontools.plugin.zsh'
zplugin snippet 'OMZ::plugins/zsh_reload/zsh_reload.plugin.zsh'
# }}}

# zplugin: Commands {{{
zplugin ice from'gh-r' as'command' mv'gotcha_* -> gotcha'; zplugin light 'b4b4r07/gotcha'
zplugin ice as'command' cp'httpstat.sh -> httpstat' pick'httpstat'; zplugin light 'b4b4r07/httpstat'

zplugin snippet --command "${HOMEBREW_PREFIX}/share/git-core/contrib/diff-highlight/diff-highlight"
# }}}

# zplugin: Completions {{{
zplugin ice pick''; zplugin light 'robbyrussell/oh-my-zsh'
zplugin ice pick''; zplugin light 'sei40kr/new-my-zsh'
zplugin ice pick''; zplugin light 'zsh-users/zsh-completions'

zplugin snippet "${ANYENV_ROOT}/completions/anyenv.zsh"
# }}}

compinit
zplugin cdreplay -q

# zplugin: Plugins loaded after compinit {{{
zplugin light 'zdharma/fast-syntax-highlighting'
zplugin light 'zsh-users/zsh-autosuggestions'

# zplugin ice pick'spaceship.zsh'; zplugin light 'denysdovhan/spaceship-zsh-theme'
zplugin ice pick'pure.zsh'; zplugin light 'sindresorhus/pure'
# }}}

