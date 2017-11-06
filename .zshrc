#!/usr/bin/env zsh

# .zshrc
# author: Seong Yong-ju ( @sei40kr )

# Launch tmux if it's installed and not running
if [[ -z "$TMUX" ]] && [[ "${+commands[tmux]}" == 1 ]]
then
  tmux new-session
  exit
fi

zmodload zsh/zpty

. "${HOME}/.zplugin/bin/zplugin.zsh"

autoload -Uz add-zsh-hook
autoload -Uz cdr
autoload -Uz chpwd
autoload -Uz chpwd_recent_dirs
autoload -Uz compinit
autoload -Uz _zplugin

# Don't use brew --prefix because it's too slow
if [[ -d "${HOME}/.linuxbrew" ]]
then
  __brew_prefix="${HOME}/.linuxbrew"
else
  __brew_prefix='/usr/local'
fi

path=(
  "${HOME}/.cabal/bin"
  "${HOME}/.cargo/bin"
  "${GOPATH}/bin"
  "${PYENV_ROOT}/bin"
  "${RBENV_ROOT}/bin"
  "${path[@]}"
)

. "${HOME}/.sdkman/bin/sdkman-init.sh"
eval "$(pyenv init - zsh --no-rehash)"
eval "$(rbenv init - zsh --no-rehash)"

if [[ "${+_comps}" == 1 ]]
then
  _comps[zplugin]=_zplugin
fi

# zplugin: Utilities {{{
zplugin snippet 'OMZ::lib/git.zsh'
zplugin snippet 'OMZ::lib/clipboard.zsh'
zplugin snippet "${HOME}/.zsh/rc/10_utilities.zsh"

zplugin snippet 'OMZ::lib/key-bindings.zsh'
zplugin snippet "${HOME}/.zsh/rc/20_key-bindings.zsh"

zplugin snippet "${HOME}/.zsh/rc/30_aliases.zsh"

zplugin snippet 'OMZ::lib/completion.zsh'
zplugin snippet 'OMZ::lib/compfix.zsh'
zplugin snippet "${HOME}/.zsh/rc/50_options.zsh"

zplugin snippet "${HOME}/.zsh/rc/70_misc.zsh"

zplugin snippet "${HOME}/.zsh/rc/80_custom.zsh"
# }}}

# zplugin: Plugins {{{
zplugin light 'b4b4r07/emoji-cli'
zplugin light 'b4b4r07/enhancd'
zplugin light 'djui/alias-tips'
zplugin light 'lukechilds/zsh-nvm'
zplugin light 'mollifier/anyframe'
zplugin light 'mollifier/cd-gitroot'
zplugin ice pick'k.sh'; zplugin light 'supercrabtree/k'

zplugin snippet 'OMZ::plugins/colored-man-pages/colored-man-pages.plugin.zsh'
zplugin snippet 'OMZ::plugins/dotenv/dotenv.plugin.zsh'
zplugin snippet 'OMZ::plugins/fancy-ctrl-z/fancy-ctrl-z.plugin.zsh'
zplugin snippet 'OMZ::plugins/git/git.plugin.zsh'
zplugin snippet 'OMZ::plugins/gitignore/gitignore.plugin.zsh'
zplugin snippet 'OMZ::plugins/gnu-utils/gnu-utils.plugin.zsh'
zplugin snippet 'OMZ::plugins/jsontools/jsontools.plugin.zsh'
zplugin snippet 'OMZ::plugins/zsh_reload/zsh_reload.plugin.zsh'

zplugin snippet "${__brew_prefix}/opt/fzf/shell/completion.zsh"
zplugin snippet "${__brew_prefix}/opt/fzf/shell/key-bindings.zsh"
# }}}

# zplugin: Commands {{{
zplugin ice from'gh-r' as'command' mv'gotcha_* -> gotcha';           zplugin light 'b4b4r07/gotcha'
zplugin ice as'command' cp'httpstat.sh -> httpstat' pick'httpstat';  zplugin light 'b4b4r07/httpstat'
zplugin ice as'command';                                             zplugin snippet "${__brew_prefix}/share/git-core/contrib/diff-highlight/diff-highlight"
# }}}

# zplugin: Completions {{{
zplugin ice blockf; zplugin light 'zsh-users/zsh-completions'
# }}}

compinit -C
zplugin cdreplay -q

# zplugin: Plugins loaded after compinit {{{
zplugin light 'zdharma/fast-syntax-highlighting'
zplugin light 'zsh-users/zsh-autosuggestions'

zplugin ice pick'spaceship.zsh'; zplugin light 'denysdovhan/spaceship-zsh-theme'
# }}}
