#!/usr/bin/env zsh

# .zshrc
# author: Seong Yong-ju ( @sei40kr )

if [[ -z "$TMUX" ]] && [[ -z "$STY" ]]
then
  . "${HOME}/.zsh/rc/exports.rc.zsh"
  . "${HOME}/.zsh/rc/tmux.rc.zsh"
fi

zmodload zsh/zpty

if [[ "${+commands[anyenv]}" == 1 ]]
then
  eval "$(anyenv init - zsh)"
fi

autoload -Uz compinit

. "${HOME}/.zplugin/bin/zplugin.zsh"

autoload -Uz _zplugin

autoload -Uz add-zsh-hook
autoload -Uz cdr
autoload -Uz chpwd_recent_dirs

if [[ "${+_comps}" == 1 ]]
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

zplugin snippet "${HOME}/.zsh/rc/70_misc.zsh"

zplugin snippet "${HOME}/.zsh/rc/80_custom.zsh"
# }}}

# zplugin: Plugins {{{

# djui/alias-tips {{{
export ZSH_PLUGINS_ALIAS_TIPS_TEXT='alias-tips: '
# }}}

# sindresorhus/pure {{{
PURE_PROMPT_SYMBOL='➔'
# }}}

zplugin light 'b4b4r07/emoji-cli'
# zplugin light 'b4b4r07/enhancd'
zplugin light 'djui/alias-tips'
zplugin light 'mollifier/anyframe'
zplugin light 'mollifier/cd-gitroot'
zplugin light 'sei40kr/zsh-tmux-rename'
zplugin ice pick'k.sh'
zplugin light 'supercrabtree/k'

zplugin snippet 'OMZ::plugins/dotenv/dotenv.plugin.zsh'
zplugin snippet 'OMZ::plugins/fancy-ctrl-z/fancy-ctrl-z.plugin.zsh'
zplugin snippet 'OMZ::plugins/git/git.plugin.zsh'
zplugin snippet 'OMZ::plugins/github/github.plugin.zsh'
zplugin snippet 'OMZ::plugins/gnu-utils/gnu-utils.plugin.zsh'
zplugin snippet 'OMZ::plugins/jsontools/jsontools.plugin.zsh'
zplugin snippet 'OMZ::plugins/zsh_reload/zsh_reload.plugin.zsh'
# }}}

# zplugin: Commands {{{
zplugin ice from'gh-r' as'command' mv'gotcha_* -> gotcha'
zplugin light 'b4b4r07/gotcha'
zplugin ice as'command' cp'httpstat.sh -> httpstat' pick'httpstat'
zplugin light 'b4b4r07/httpstat'

zplugin snippet --command \
    'https://raw.githubusercontent.com/jonas/tig/master/contrib/tig-pick'
zplugin snippet --command \
    'https://raw.githubusercontent.com/Russell91/sshrc/master/moshrc'
zplugin snippet --command \
    'https://raw.githubusercontent.com/Russell91/sshrc/master/sshrc'
zplugin snippet --command \
    "${HOMEBREW_PREFIX}/share/git-core/contrib/diff-highlight/diff-highlight"
# }}}

# zplugin: Completions {{{
zplugin ice pick''
zplugin light 'jsforce/jsforce-zsh-completions'
zplugin ice pick''
zplugin light 'zsh-users/zsh-completions'
# }}}

compinit
zplugin cdreplay -q

# zplugin: Plugins loaded after compinit {{{
zplugin ice wait'1' atload'_zsh_highlight'
zplugin light 'zdharma/fast-syntax-highlighting'
zplugin ice wait'1' atload'_zsh_autosuggest_start'
zplugin light 'zsh-users/zsh-autosuggestions'

zplugin ice pick'spaceship.zsh' wait'!0'
zplugin light 'denysdovhan/spaceship-zsh-theme'
# }}}

